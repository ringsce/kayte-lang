// Jenkinsfile for Kayte Project on macOS, Linux (multi-architecture with Lazarus)

pipeline {
    agent any // Jenkins will select agents based on labels used in stages.

    environment {
        APP_NAME_MAIN = 'Kayte'          // Main executable from Kayte.lpr
        APP_NAME_INTERPRETER = 'vb6interpreter' // Executable from vb6interpreter.lpr
        BUILD_DIR = '.'                     // Adjust if your .lpr files are in a subfolder
        // FPC_TARGET is derived from OS/ARCH for lazbuild flags
        UNIVERSAL_APP_NAME = "${APP_NAME_MAIN}-macos-universal" // Name for the combined universal binary
    }

    stages {
        stage('Checkout Source Code') {
            steps {
                echo 'Checking out source code…'
                git branch: 'main', url: 'https://github.com/ringsce/kayte-lang.git'
                echo 'Source code checkout complete.'
            }
        }

        // Dedicated stage for building the macOS Universal Binary using lazbuild and lipo
        stage('Build macOS Universal Binary') {
            // This stage requires a macOS agent. An Intel Mac can cross-compile to ARM64.
            agent { label 'macos-x86_64' } // Replace with your macOS Intel agent label

            steps {
                script {
                    echo "Building macOS Universal Binary for ${APP_NAME_MAIN}..."

                    dir("${BUILD_DIR}") {
                        // Clean previous Lazarus builds
                        echo "Cleaning previous Lazarus builds for macOS..."
                        sh "lazbuild --clean Kayte.lpr"

                        // Build for x86_64
                        echo "Building ${APP_NAME_MAIN} for macOS x86_64..."
                        sh "lazbuild Kayte.lpr --build-mode=Release --os=darwin --cpu=x86_64"
                        // Lazbuild outputs to 'lib/<project_name>/<os>-<cpu>/<project_name>'
                        // Example path: 'lib/Kayte/darwin-x86_64/KayteApp'
                        String x86_64_binary_path = "lib/Kayte/darwin-x86_64/${APP_NAME_MAIN}"
                        if (!fileExists(x86_64_binary_path)) {
                            error "macOS x86_64 binary not found at ${x86_64_binary_path}"
                        }

                        // Build for arm64
                        echo "Building ${APP_NAME_MAIN} for macOS arm64..."
                        sh "lazbuild Kayte.lpr --build-mode=Release --os=darwin --cpu=aarch64"
                        // Example path: 'lib/Kayte/darwin-aarch64/KayteApp'
                        String arm64_binary_path = "lib/Kayte/darwin-aarch64/${APP_NAME_MAIN}"
                        if (!fileExists(arm64_binary_path)) {
                            error "macOS arm64 binary not found at ${arm64_binary_path}"
                        }

                        // Create Universal Binary using lipo
                        echo "Creating universal binary: ${UNIVERSAL_APP_NAME}..."
                        sh "lipo -create -output \"${UNIVERSAL_APP_NAME}\" \"${x86_64_binary_path}\" \"${arm64_binary_path}\""

                        // Make the universal binary executable
                        sh "chmod +x \"${UNIVERSAL_APP_NAME}\""

                        echo "macOS Universal Binary build completed: ${UNIVERSAL_APP_NAME}"
                    }
                }
            }
            post {
                success { steps { echo "macOS Universal Binary build succeeded!" } }
                failure { steps { echo "macOS Universal Binary build failed." } }
            }
        }

        // Matrix build stage for individual OS/ARCH combinations (Linux, and potentially macOS if not universal)
        stage('Build & Test Matrix (Linux + Individual macOS)') {
            matrix {
                axes {
                    axis { name 'OS'; values 'macos', 'linux' }
                    axis { name 'ARCH'; values 'amd64', 'arm64' }
                }
                exclude {
                    // Exclude macOS builds from this matrix if the 'Build macOS Universal Binary' stage covers all macOS needs.
                    // This prevents redundant builds. If you want separate macos-amd64 and macos-arm64
                    // *individual* binaries (not universal) from lazbuild, then don't exclude macos here.
                    axis { name 'OS'; values 'macos' }
                    axis { name 'ARCH'; values 'arm64' } // Keep this if your macOS agent is Intel only for this matrix.
                }

                stages {
                    stage('Configure Target for Matrix Build') {
                        steps {
                            script {
                                // Map matrix axes to lazbuild --os and --cpu flags
                                String targetOS = env.OS == 'macos' ? 'darwin' : env.OS
                                String targetCPU = env.ARCH == 'amd64' ? 'x86_64' : 'aarch64'

                                env.LAZBUILD_OS_FLAG = targetOS
                                env.LAZBUILD_CPU_FLAG = targetCPU

                                env.CURRENT_MAIN_APP_NAME = "${APP_NAME_MAIN}-${env.OS}-${env.ARCH}"
                                env.CURRENT_INTERPRETER_APP_NAME = "${APP_NAME_INTERPRETER}-${env.OS}-${env.ARCH}"

                                echo "Configured → OS=${env.OS} ARCH=${env.ARCH} LAZBUILD_OS=${env.LAZBUILD_OS_FLAG} LAZBUILD_CPU=${env.LAZBUILD_CPU_FLAG}"
                            }
                        }
                    }

                    stage('Build Individual Binaries') {
                        // Agent selection for matrix. Recommended to use specific agents for each combination.
                        // Example: agent { label "${env.OS}-${env.ARCH}" }
                        // For simplicity, keeping 'agent any' from global, but this relies on Jenkins correctly assigning agents.
                        agent any // Ensure agents with relevant OS/ARCH and Lazarus are available

                        steps {
                            dir("${BUILD_DIR}") {
                                echo "Building ${APP_NAME_MAIN} for ${env.OS}-${env.ARCH}..."
                                sh "lazbuild ${APP_NAME_MAIN}.lpr --build-mode=Release --os=${LAZBUILD_OS_FLAG} --cpu=${LAZBUILD_CPU_FLAG}"
                                sh "mv lib/${APP_NAME_MAIN}/${LAZBUILD_OS_FLAG}-${LAZBUILD_CPU_FLAG}/${APP_NAME_MAIN} ${CURRENT_MAIN_APP_NAME}"
                                sh "chmod +x ${CURRENT_MAIN_APP_NAME}"
                                echo "${APP_NAME_MAIN} build completed."

                                echo "Building ${APP_NAME_INTERPRETER} for ${env.OS}-${env.ARCH}..."
                                sh "lazbuild ${APP_NAME_INTERPRETER}.lpr --build-mode=Release --os=${LAZBUILD_OS_FLAG} --cpu=${LAZBUILD_CPU_FLAG}"
                                sh "mv lib/${APP_NAME_INTERPRETER}/${LAZBUILD_OS_FLAG}-${LAZBUILD_CPU_FLAG}/${APP_NAME_INTERPRETER} ${CURRENT_INTERPRETER_APP_NAME}"
                                sh "chmod +x ${CURRENT_INTERPRETER_APP_NAME}"
                                echo "${APP_NAME_INTERPRETER} build completed."

                                // If you have a separate Makefile for other components:
                                // echo "Running additional 'make' steps (if any)..."
                                // sh "make all" // Adjust this if your Makefile needs specific FPC_TARGET
                            }
                        }
                    }

                    stage('Test Individual Binaries') {
                        when {
                            expression {
                                // Assuming 'make test' can run the tests for the current OS/ARCH or you have a test script
                                return fileExists('Makefile') && sh(returnStatus: true, script: 'grep -q "^test:" Makefile') == 0
                            }
                        }
                        steps {
                            echo "Running tests for ${env.OS}-${env.ARCH}..."
                            sh 'make test' // Adjust if you have a specific test runner for Lazarus projects
                        }
                        post {
                            always {
                                steps {
                                    echo "Tests finished for ${env.OS}-${env.ARCH} (regardless of result)."
                                    // junit "test-results/${env.OS}-${env.ARCH}/**/*.xml" // Uncomment for JUnit reports
                                }
                            }
                        }
                    }
                } // End of inner 'stages' for matrix
            } // End of 'matrix' block
        } // End of 'Build & Test Matrix' stage

        stage('Archive All Built Artifacts') {
            steps {
                echo 'Archiving all built executables...'
                // Archive all binaries created by the matrix builds and the universal macOS binary.
                archiveArtifacts artifacts: "${APP_NAME_MAIN}-*-*, ${APP_NAME_INTERPRETER}-*-*, ${UNIVERSAL_APP_NAME}", onlyIfSuccessful: true
                // If the universal build is a .app bundle:
                // archiveArtifacts artifacts: "${APP_NAME_MAIN}-*-*, ${APP_NAME_INTERPRETER}-*-*, ${UNIVERSAL_APP_NAME}.app", onlyIfSuccessful: true
                echo 'Artifacts archived.'
            }
        }

        stage('Clean Workspace') {
            steps {
                echo 'Cleaning up the Jenkins workspace...'
                cleanWs() // Cleans the entire Jenkins workspace
                echo 'Workspace cleaned.'
            }
        }
    }

    post { // Global post-build actions for the entire pipeline
        always  { steps { echo 'Pipeline finished (always).' } }
        success { steps { echo 'Pipeline SUCCESS.' } }
        failure {
            steps {
                echo 'Pipeline FAILED — see console.'
                // mail to: 'your-email@example.com',
                //      subject: "Kayte CI Build FAILED: ${env.JOB_NAME} #${env.BUILD_NUMBER}",
                //      body: "The Jenkins CI build for Kayte project failed. View details at: ${env.BUILD_URL}"
            }
        }
        unstable{ steps { echo 'Pipeline UNSTABLE.' } }
        // aborted { steps { echo 'Pipeline ABORTED!' } }
    }
}