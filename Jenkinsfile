// Jenkinsfile for Kayte Project on macOS, Linux (multi-architecture)

pipeline {
    // 'agent any' allows Jenkins to select the appropriate agent based on the matrix axis.
    // Ensure you have agents with the necessary OS and architecture capabilities, and labels if you use specific ones.
    agent any

    // Environment variables that are global to the entire pipeline execution.
    environment {
        APP_NAME_BASE = 'KayteApp' // Base name for your executable (e.g., KayteApp-macos-amd64)
        BUILD_DIR = '.'            // Relative path to your Makefile (e.g., '.' for repository root)
        // No FPC_TARGET here, it will be set dynamically in the matrix stage.
    }

    stages {
        stage('Checkout Source Code') {
            steps {
                echo 'Checking out source code...'
                // Replace 'your-git-credentials-id' and 'https://github.com/your-org/kayte.git'
                git branch: 'main', credentialsId: 'your-git-credentials-id', url: 'https://github.com/your-org/kayte.git'
                echo 'Source code checkout complete.'
            }
        }

        stage('Build and Test Matrix') {
            matrix {
                axes {
                    // Define the OS axis
                    axis {
                        name 'OS'
                        values 'macos', 'linux'
                    }
                    // Define the ARCH (architecture) axis
                    axis {
                        name 'ARCH'
                        values 'amd64', 'arm64'
                    }
                }
                // Exclude invalid or currently unsupported combinations.
                // Adjust these 'when' conditions based on your actual agents and FPC cross-compilation capabilities.
                exclude {
                    // Example: Exclude if you don't have a macOS ARM64 agent OR FPC cannot cross-compile to it.
                    // This is for illustration; you'll need working agents for each combo.
                    // When selecting agents for specific OS/ARCH, ensure the agent's label matches.
                    // For instance, an agent for macOS ARM64 might have label 'macos-arm64'.
                    // You might need to set 'agent { label "${OS}-${ARCH}" }' inside the matrix stages for specific agents.
                    // Or, if your agent can cross-compile, 'agent any' and FPC_TARGET handling is enough.
                    // For now, let's assume 'agent any' is sufficient and FPC handles targets.

                    // If a specific OS/ARCH combination is not buildable or testable, exclude it here.
                    // Example:
                    // axis { name 'OS'; values 'macos' }
                    // axis { name 'ARCH'; values 'arm64' }
                    // agent { label 'your-x86-linux-agent' } // If this agent can't build macos-arm64
                }

                // Stages that will run for each combination in the matrix
                stages {
                    stage('Configure Target') {
                        steps {
                            script {
                                // Dynamically set FPC_TARGET based on current OS and ARCH combination
                                if (env.OS == 'macos') {
                                    if (env.ARCH == 'amd64') {
                                        env.FPC_TARGET = 'x86_64-darwin'
                                    } else if (env.ARCH == 'arm64') {
                                        env.FPC_TARGET = 'aarch64-darwin' // Or 'arm64-darwin' depending on FPC version
                                    }
                                } else if (env.OS == 'linux') {
                                    if (env.ARCH == 'amd64') {
                                        env.FPC_TARGET = 'x86_64-linux'
                                    } else if (env.ARCH == 'arm64') {
                                        env.FPC_TARGET = 'aarch64-linux' // Or 'arm-linux' depending on FPC version
                                    }
                                }
                                // Construct the final executable name for this specific build
                                env.CURRENT_APP_NAME = "${APP_NAME_BASE}-${env.OS}-${env.ARCH}"

                                echo "Configuring build for OS: ${env.OS}, ARCH: ${env.ARCH}, FPC_TARGET: ${env.FPC_TARGET}"
                                echo "Final executable name will be: ${env.CURRENT_APP_NAME}"
                            }
                        }
                    }

                    stage('Build') {
                        steps {
                            dir("${BUILD_DIR}") {
                                echo "Building ${APP_NAME_BASE} for ${env.OS}-${env.ARCH}..."
                                // Pass FPC_TARGET to make. Your Makefile must be set up to use this variable.
                                // E.g., in Makefile: $(FPC) -T$(FPC_TARGET) ...
                                sh "make all FPC_TARGET=${env.FPC_TARGET}"
                                // Rename the compiled binary to include its OS and ARCH for archiving later
                                sh "mv ${APP_NAME_BASE} ${env.CURRENT_APP_NAME}"
                                echo 'Build completed.'
                            }
                        }
                    }

                    stage('Test') {
                        // This stage will only run if your Makefile has a 'test:' target.
                        when {
                            expression {
                                return fileExists('Makefile') && sh(returnStatus: true, script: 'grep -q "^test:" Makefile') == 0
                            }
                        }
                        steps {
                            echo "Running tests for ${env.OS}-${env.ARCH}..."
                            // Assuming 'make test' is universal and runs tests for the current FPC_TARGET.
                            // If tests need to be run natively on the target architecture (e.g., ARM64 tests on an ARM64 machine),
                            // you might need a more complex setup with dedicated test agents or remote execution.
                            sh 'make test'
                            echo 'Tests finished.'
                        }
                        post {
                            always {
                                steps {
                                    echo "Tests finished for ${env.OS}-${env.ARCH} (regardless of result)."
                                    // Uncomment if you output JUnit-compatible results
                                    // junit "test-results/${env.OS}-${env.ARCH}/**/*.xml"
                                }
                            }
                        }
                    }
                } // <-- This 'stages' block ends here
            } // <-- This 'matrix' block ends here
        }

        // The duplicate stage starts below here. Remove it.
        // stage('Build and Test Matrix') {
        //     matrix {
        //         axes { // <-- This 'axes' block starts here
        //             // Define the OS axis
        //             axis {
        //                 name 'OS'
        //                 values 'macos', 'linux'
        //             }
        //             // Define the ARCH (architecture) axis
        //             axis {
        //                 name 'ARCH'
        //                 values 'amd64', 'arm64'
        //             }
        //         } // <-- This 'axes' block ends here
        //         exclude {
        //             // ... exclusions ...
        //         }
        //         stages {
        //             // ... configure target ...
        //             // ... build ...
        //             // ... test ...
        //         }
        //     }
        // }


        stage('Archive All Built Artifacts') {
            // This stage runs once after all matrix combinations have completed.
            steps {
                echo 'Archiving all built executables from the matrix...'
                // Archive all binaries created by the matrix builds.
                // The pattern uses wildcards to match the renamed executables (e.g., KayteApp-macos-amd64).
                archiveArtifacts artifacts: "${APP_NAME_BASE}-*-*", onlyIfSuccessful: true
                echo 'Artifacts archived.'
            }
        }

        stage('Clean Workspace') {
            // This stage ensures the workspace is clean after archiving.
            steps {
                echo 'Cleaning up the Jenkins workspace...'
                // cleanWs() is a built-in Jenkins step to clean the entire workspace.
                cleanWs()
                echo 'Workspace cleaned.'
            }
        }
    }

    post {
        // Post-build actions: These run once for the entire pipeline, after all stages (including matrix) complete.
        always {
            steps {
                echo 'Pipeline execution completed.'
            }
        }
        success {
            steps {
                echo 'Overall pipeline: SUCCESS!'
                // Add global success notifications here if needed (e.g., email to team)
            }
        }
        failure {
            steps {
                echo 'Overall pipeline: FAILED! Check console output for details.'
                // Add global failure notifications here (e.g., email, Slack, Teams)
                // mail to: 'your-email@example.com',
                //      subject: "Kayte CI Build FAILED: ${env.JOB_NAME} #${env.BUILD_NUMBER}",
                //      body: "The Jenkins CI build for Kayte project failed. View details at: ${env.BUILD_URL}"
            }
        }
        unstable {
            steps {
                echo 'Overall pipeline: UNSTABLE (e.g., some tests failed, or warnings occurred).'
            }
        }
        // aborted {
        //     steps {
        //         echo 'Overall pipeline: ABORTED!'
        //     }
        // }
    }
}