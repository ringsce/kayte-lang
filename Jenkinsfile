// Jenkinsfile for Kayte Project on macOS, Linux (multi-architecture with Free Pascal Compiler)

pipeline {
    agent any // Default agent, stages will specify more precise labels

    environment {
        // All environment variables must be defined INSIDE this block
        // Assuming /usr/local/bin is where FPC is installed on macOS.
        // For Linux agents, you might need to adjust this PATH based on their installation.
        PATH = "/usr/local/bin:$PATH" 

        PROJECT_NAME_KAYTE = 'Kayte'
        PROJECT_NAME_VB6_INTERPRETER = 'vb6interpreter'

        // Define the common build directory relative to the workspace root
        // This indicates your .lpr files are located in a 'projects' subfolder.
        BUILD_DIR = 'projects' // This is the subdirectory containing your .lpr files
        
        // Define the output directory for consolidated binaries, relative to workspace root
        OUTPUT_BIN_DIR = "build_artifacts"

        // Name for the combined universal binary for Kayte
        UNIVERSAL_APP_NAME_KAYTE = "${PROJECT_NAME_KAYTE}-macos-universal"
        // Name for the combined universal binary for vb6interpreter (if needed, otherwise just build both arch)
        UNIVERSAL_APP_NAME_VB6_INTERPRETER = "${PROJECT_NAME_VB6_INTERPRETER}-macos-universal"
    }

    stages {
        stage('Prepare Workspace') {
            steps {
                echo 'Cleaning up existing workspace and creating output directory...'
                cleanWs() // Start with a clean workspace
                sh "mkdir -p ${OUTPUT_BIN_DIR}"
            }
        }

        stage('Checkout Source Code') {
            steps {
                echo 'Checking out source code…'
                git branch: 'main', url: 'https://github.com/ringsce/kayte-lang.git'
                echo 'Source code checkout complete.'
            }
        }

        // Dedicated stage for building macOS binaries, including universal for Kayte
        stage('Build macOS Binaries (Universal for Kayte)') {
            // This stage requires a macOS agent.
            // IMPORTANT: Ensure you have a Jenkins agent (node) configured with the label 'macos'.
            // This macOS agent must have FPC installed, along with its cross-compilers for both
            // x86_64 and aarch64 (ARM64) to perform the universal build. An Intel Mac can cross-compile
            // to ARM64, and an Apple Silicon Mac can typically cross-compile to x86_64 (via Rosetta for FPC).
            agent { label 'macos' } 

            steps {
                script {
                    echo "Building macOS binaries for ${PROJECT_NAME_KAYTE} and ${PROJECT_NAME_VB6_INTERPRETER} using FPC..."

                    // Define temporary build directories for FPC output to prevent conflicts
                    // These are relative to the current workspace root (./)
                    String kayte_x86_64_fpc_out_dir = "./fpc_build_temp/${PROJECT_NAME_KAYTE}/darwin-x86_64"
                    String kayte_arm64_fpc_out_dir = "./fpc_build_temp/${PROJECT_NAME_KAYTE}/darwin-aarch64"
                    String vb6_x86_64_fpc_out_dir = "./fpc_build_temp/${PROJECT_NAME_VB6_INTERPRETER}/darwin-x86_64"
                    String vb6_arm64_fpc_out_dir = "./fpc_build_temp/${PROJECT_NAME_VB6_INTERPRETER}/darwin-aarch64"
                    
                    sh "mkdir -p ${kayte_x86_64_fpc_out_dir}"
                    sh "mkdir -p ${kayte_arm64_fpc_out_dir}"
                    sh "mkdir -p ${vb6_x86_64_fpc_out_dir}"
                    sh "mkdir -p ${vb6_arm64_fpc_out_dir}"


                    // --- Build Kayte for x86_64 ---
                    echo "Building ${PROJECT_NAME_KAYTE} for macOS x86_64..."
                    // FPC command runs from workspace root (./), reads projects/Kayte.lpr, outputs to temp dir
                    sh "fpc ${BUILD_DIR}/${PROJECT_NAME_KAYTE}.lpr -B -O3 -Tdarwin -Px86_64 -FE${kayte_x86_64_fpc_out_dir} -FU${kayte_x86_64_fpc_out_dir}"
                    // Move compiled executable to final output directory
                    sh "mv ${kayte_x86_64_fpc_out_dir}/${PROJECT_NAME_KAYTE} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-x86_64"
                    sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-x86_64"


                    // --- Build Kayte for arm64 ---
                    echo "Building ${PROJECT_NAME_KAYTE} for macOS arm64..."
                    sh "fpc ${BUILD_DIR}/${PROJECT_NAME_KAYTE}.lpr -B -O3 -Tdarwin -Paarch64 -FE${kayte_arm64_fpc_out_dir} -FU${kayte_arm64_fpc_out_dir}"
                    sh "mv ${kayte_arm64_fpc_out_dir}/${PROJECT_NAME_KAYTE} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-arm64"
                    sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-arm64"


                    // --- Create Universal Binary for Kayte using lipo ---
                    echo "Creating universal binary: ${UNIVERSAL_APP_NAME_KAYTE}..."
                    sh "lipo -create -output \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_KAYTE}\" " +
                       "\"${kayte_x86_64_fpc_out_dir}/${PROJECT_NAME_KAYTE}\" " + 
                       "\"${kayte_arm64_fpc_out_dir}/${PROJECT_NAME_KAYTE}\""
                    sh "chmod +x \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_KAYTE}\""
                    echo "${UNIVERSAL_APP_NAME_KAYTE} build completed."


                    // --- Build vb6interpreter for x86_64 ---
                    echo "Building ${PROJECT_NAME_VB6_INTERPRETER} for macOS x86_64..."
                    sh "fpc ${BUILD_DIR}/${PROJECT_NAME_VB6_INTERPRETER}.lpr -B -O3 -Tdarwin -Px86_64 -FE${vb6_x86_64_fpc_out_dir} -FU${vb6_x86_64_fpc_out_dir}"
                    sh "mv ${vb6_x86_64_fpc_out_dir}/${PROJECT_NAME_VB6_INTERPRETER} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-x86_64"
                    sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-x86_64"


                    // --- Build vb6interpreter for arm64 ---
                    echo "Building ${PROJECT_NAME_VB6_INTERPRETER} for macOS arm64..."
                    sh "fpc ${BUILD_DIR}/${PROJECT_NAME_VB6_INTERPRETER}.lpr -B -O3 -Tdarwin -Paarch64 -FE${vb6_arm64_fpc_out_dir} -FU${vb6_arm64_fpc_out_dir}"
                    sh "mv ${vb6_arm64_fpc_out_dir}/${PROJECT_NAME_VB6_INTERPRETER} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-arm64"
                    sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-arm64"

                    // If you also want a universal binary for vb6interpreter, uncomment and adjust this:
                    // echo "Creating universal binary: ${UNIVERSAL_APP_NAME_VB6_INTERPRETER}..."
                    // sh "lipo -create -output \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_VB6_INTERPRETER}\" " +
                    //    "\"${vb6_x86_64_fpc_out_dir}/${PROJECT_NAME_VB6_INTERPRETER}\" " +
                    //    "\"${vb6_arm64_fpc_out_dir}/${PROJECT_NAME_VB6_INTERPRETER}\""
                    // sh "chmod +x \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_VB6_INTERPRETER}\""
                    // echo "${UNIVERSAL_APP_NAME_VB6_INTERPRETER} build completed."

                    echo "macOS native and universal builds completed."
                }
            }
            post {
                success { steps { echo "macOS builds succeeded!" } }
                failure { steps { echo "macOS builds failed." } }
                always { // Clean up temporary FPC build directories for macOS
                    steps {
                        // Using PROJECT_NAME to remove the top-level directory for each project's temp files
                        sh "rm -rf ./fpc_build_temp/${PROJECT_NAME_KAYTE}"
                        sh "rm -rf ./fpc_build_temp/${PROJECT_NAME_VB6_INTERPRETER}"
                    }
                }
            }
        }

        // Matrix build stage for Linux builds
        stage('Build & Test Linux Binaries') {
            matrix {
                axes {
                    axis { name 'ARCH'; values 'amd64', 'arm64' }
                }
                // IMPORTANT: Ensure you have Jenkins agents configured with the labels:
                // - 'linux-amd64': A Linux agent with FPC installed (and optionally cross-compilers if needed by other stages).
                // - 'linux-arm64': A Linux ARM64 agent with FPC installed.
                agent { label "linux-${ARCH}" } 

                stages {
                    stage('Configure Target for Linux Build') {
                        steps {
                            script {
                                // Map matrix axes to fpc --os and --cpu flags
                                String targetOS = 'linux'
                                String targetCPU = env.ARCH == 'amd64' ? 'x86_64' : 'aarch64'

                                env.FPC_OS_FLAG = targetOS
                                env.FPC_CPU_FLAG = targetCPU

                                env.CURRENT_MAIN_APP_NAME = "${PROJECT_NAME_KAYTE}-${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}"
                                env.CURRENT_INTERPRETER_APP_NAME = "${PROJECT_NAME_VB6_INTERPRETER}-${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}"

                                echo "Configured for Linux → OS=${env.FPC_OS_FLAG} CPU=${env.FPC_CPU_FLAG}"
                            }
                        }
                    }

                    stage('Build Linux Binaries') {
                        steps {
                            script {
                                // Define temporary build directories for FPC output
                                String kayte_fpc_out_dir = "./fpc_build_temp/${PROJECT_NAME_KAYTE}/${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}"
                                String vb6_fpc_out_dir = "./fpc_build_temp/${PROJECT_NAME_VB6_INTERPRETER}/${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}"
                                
                                sh "mkdir -p ${kayte_fpc_out_dir}"
                                sh "mkdir -p ${vb6_fpc_out_dir}"
                            }


                            echo "Building ${PROJECT_NAME_KAYTE} for ${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}..."
                            sh "fpc ${BUILD_DIR}/${PROJECT_NAME_KAYTE}.lpr -B -O3 -T${FPC_OS_FLAG} -P${FPC_CPU_FLAG} -FE${kayte_fpc_out_dir} -FU${kayte_fpc_out_dir}"
                            sh "mv ${kayte_fpc_out_dir}/${PROJECT_NAME_KAYTE} ${OUTPUT_BIN_DIR}/${CURRENT_MAIN_APP_NAME}"
                            sh "chmod +x ${OUTPUT_BIN_DIR}/${CURRENT_MAIN_APP_NAME}"
                            echo "${PROJECT_NAME_KAYTE} build completed."

                            echo "Building ${PROJECT_NAME_VB6_INTERPRETER} for ${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}..."
                            sh "fpc ${BUILD_DIR}/${PROJECT_NAME_VB6_INTERPRETER}.lpr -B -O3 -T${FPC_OS_FLAG} -P${FPC_CPU_FLAG} -FE${vb6_fpc_out_dir} -FU${vb6_fpc_out_dir}"
                            sh "mv ${vb6_fpc_out_dir}/${PROJECT_NAME_VB6_INTERPRETER} ${OUTPUT_BIN_DIR}/${CURRENT_INTERPRETER_APP_NAME}"
                            sh "chmod +x ${OUTPUT_BIN_DIR}/${CURRENT_INTERPRETER_APP_NAME}"
                            echo "${PROJECT_NAME_VB6_INTERPRETER} build completed."
                        }
                    }

                    stage('Test Linux Binaries') {
                        when {
                            expression {
                                // Check if a test script or Makefile exists and can be executed for this job
                                return fileExists('tests/run_linux_tests.sh') // Example: a dedicated test script
                            }
                        }
                        steps {
                            echo "Running tests for ${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}..."
                            sh "tests/run_linux_tests.sh ${OUTPUT_BIN_DIR}/${CURRENT_MAIN_APP_NAME} ${OUTPUT_BIN_DIR}/${CURRENT_INTERPRETER_APP_NAME}"
                        }
                        post {
                            always {
                                steps {
                                    echo "Tests finished for ${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG} (regardless of result)."
                                    // junit "test-results/${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}/**/*.xml" // Uncomment for JUnit reports
                                    
                                    // Clean up intermediate FPC build directories for this matrix axis
                                    sh "rm -rf ./fpc_build_temp/${PROJECT_NAME_KAYTE}/${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}"
                                    sh "rm -rf ./fpc_build_temp/${PROJECT_NAME_VB6_INTERPRETER}/${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG}"
                                }
                            }
                        }
                    }
                } // End of inner 'stages' for matrix
            } // End of 'matrix' block
        } // End of 'Build & Test Linux Binaries' stage

        stage('Archive All Built Artifacts') {
            steps {
                echo 'Archiving all built executables...'
                // Archive all binaries from the consolidated output directory.
                archiveArtifacts artifacts: "${OUTPUT_BIN_DIR}/*", onlyIfSuccessful: true
                echo 'Artifacts archived.'
            }
        }
    }

    post { // Global post-build actions for the entire pipeline
        always  { steps { echo 'Pipeline finished (always).' } }
        success { steps { echo 'Pipeline SUCCESS.' } }
        failure {
            steps {
                echo 'Pipeline FAILED — see console.'
                // mail to: 'pdvicentel@gleentech.com',
                //      subject: "Kayte CI Build FAILED: ${env.JOB_NAME} #${env.BUILD_NUMBER}",
                //      body: "The Jenkins CI build for Kayte project failed. View details at: ${env.BUILD_URL}"
            }
        }
        unstable{ steps { echo 'Pipeline UNSTABLE.' } }
        // aborted { steps { echo 'Pipeline ABORTED!' } }
    }
}
