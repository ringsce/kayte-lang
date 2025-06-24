// Jenkinsfile for Kayte Project on macOS, Linux (multi-architecture with Free Pascal Compiler)

pipeline {
    agent any // Default agent, stages will specify more precise labels

    environment {
        // Global PATH: Only include /usr/local/bin for general tools.
        // lazbuild path will be added specifically for the macOS agent.
        PATH = "/usr/local/bin:$PATH" 

        PROJECT_NAME_KAYTE = 'Kayte'
        PROJECT_NAME_VB6_INTERPRETER = 'vb6interpreter'

        // Define the subdirectory where your .lpr/.lpi files are located
        LPR_DIR = 'projects' // This variable will now point to where both .lpr and .lpi files are.
        
        // Define the output directory for consolidated binaries, relative to workspace root
        OUTPUT_BIN_DIR = "build_artifacts" // Binaries will go here

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

        stage('Build macOS Binaries (Universal for Kayte)') {
            // This stage requires a macOS agent.
            // IMPORTANT: Ensure you have a Jenkins agent (node) configured with the label 'macos'.
            // This macOS agent must have Lazarus installed (including lazbuild)
            agent { label 'macos' } 

            steps {
                // Use withEnv to add lazbuild's path only for this stage
                withEnv(["PATH=/Applications/lazarus:$PATH"]) {
                    script {
                        echo "Building macOS binaries for ${PROJECT_NAME_KAYTE} and ${PROJECT_NAME_VB6_INTERPRETER} using lazbuild..."

                        // Define where lazbuild will put its temporary output files *within the project directory*
                        // lazbuild typically creates `lib/<os>-<cpu>/` inside the .lpi's directory (i.e., 'projects').
                        String kayte_x86_64_laz_out_subpath = "${LPR_DIR}/lib/darwin-x86_64"
                        String kayte_arm64_laz_out_subpath = "${LPR_DIR}/lib/darwin-aarch64"
                        String vb6_x86_64_laz_out_subpath = "${LPR_DIR}/lib/darwin-x86_64"
                        String vb6_arm64_laz_out_subpath = "${LPR_DIR}/lib/darwin-aarch64"
                        
                        // No need to explicitly create these temporary directories; lazbuild will create them.
                        // Ensure the final OUTPUT_BIN_DIR exists (already done in Prepare Workspace, but good to be explicit)
                        sh "mkdir -p ${OUTPUT_BIN_DIR}"


                        // Use 'dir' to change into the workspace root so lazbuild can find project files
                        dir('.') { // Execute lazbuild commands from the repository root
                            // --- DIAGNOSTIC STEPS: Check the LPI file ---
                            // CHANGED TO .lpi
                            echo "Verifying ${LPR_DIR}/${PROJECT_NAME_KAYTE}.lpr existence and content..."
                            sh "ls -la ${LPR_DIR}/${PROJECT_NAME_KAYTE}.lpr"
                            sh "cat ${LPR_DIR}/${PROJECT_NAME_KAYTE}.lpr"
                            // --- END DIAGNOSTIC STEPS ---

                            // --- Build Kayte for x86_64 ---
                            echo "Building ${PROJECT_NAME_KAYTE} for macOS x86_64..."
                            // CHANGED TO .lpi
                            sh "lazbuild --lazarusdir=/Applications/lazarus --os=darwin --cpu=x86_64 ${LPR_DIR}/${PROJECT_NAME_KAYTE}.lpi" 
                            // Move compiled executable from lazbuild's default output location
                            sh "mv ${kayte_x86_64_laz_out_subpath}/${PROJECT_NAME_KAYTE} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-x86_64"
                            sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-x86_64"


                            // --- Build Kayte for arm64 ---
                            echo "Building ${PROJECT_NAME_KAYTE} for macOS arm64..."
                            // CHANGED TO .lpi
                            sh "lazbuild --lazarusdir=/Applications/lazarus --os=darwin --cpu=aarch64 ${LPR_DIR}/${PROJECT_NAME_KAYTE}.lpi"
                            sh "mv ${kayte_arm64_laz_out_subpath}/${PROJECT_NAME_KAYTE} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-arm64"
                            sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-arm64"


                            // --- Create Universal Binary for Kayte using lipo ---
                            echo "Creating universal binary: ${UNIVERSAL_APP_NAME_KAYTE}..."
                            sh "lipo -create -output \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_KAYTE}\" " +
                               "\"${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-x86_64\" " + 
                               "\"${OUTPUT_BIN_DIR}/${PROJECT_NAME_KAYTE}-macos-arm64\""
                            sh "chmod +x \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_KAYTE}\""
                            echo "${UNIVERSAL_APP_NAME_KAYTE} build completed."


                            // --- Build vb6interpreter for x86_64 ---
                            echo "Building ${PROJECT_NAME_VB6_INTERPRETER} for macOS x86_64..."
                            // CHANGED TO .lpi
                            sh "lazbuild --lazarusdir=/Applications/lazarus --os=darwin --cpu=x86_64 ${LPR_DIR}/${PROJECT_NAME_VB6_INTERPRETER}.lpi"
                            sh "mv ${vb6_x86_64_laz_out_subpath}/${PROJECT_NAME_VB6_INTERPRETER} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-x86_64"
                            sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-x86_64"


                            // --- Build vb6interpreter for arm64 ---
                            echo "Building ${PROJECT_NAME_VB6_INTERPRETER} for macOS arm64..."
                            // CHANGED TO .lpi
                            sh "lazbuild --lazarusdir=/Applications/lazarus --os=darwin --cpu=aarch64 ${LPR_DIR}/${PROJECT_NAME_VB6_INTERPRETER}.lpi"
                            sh "mv ${vb6_arm64_laz_out_subpath}/${PROJECT_NAME_VB6_INTERPRETER} ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-arm64"
                            sh "chmod +x ${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-arm64"

                            // If you also want a universal binary for vb6interpreter, uncomment and adjust this:
                            // echo "Creating universal binary: ${UNIVERSAL_APP_NAME_VB6_INTERPRETER}..."
                            // sh "lipo -create -output \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_VB6_INTERPRETER}\" " +
                            //    "\"${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-x86_64\" " +
                            //    "\"${OUTPUT_BIN_DIR}/${PROJECT_NAME_VB6_INTERPRETER}-macos-arm64\""
                            // sh "chmod +x \"${OUTPUT_BIN_DIR}/${UNIVERSAL_APP_NAME_VB6_INTERPRETER}\""
                            // echo "${UNIVERSAL_APP_NAME_VB6_INTERPRETER} build completed."

                            echo "macOS native and universal builds completed."
                        } // End of dir('.') block
                    } // End of script block
                } // End of withEnv block
            } // End of steps block for macOS stage
            post {
                success { steps { echo "macOS builds succeeded!" } }
                failure { steps { echo "macOS builds failed." } }
                always { // Clean up temporary lazbuild directories (within LPR_DIR)
                    steps {
                        // lazbuild's default output is typically 'lib' subdirectory within the project folder.
                        sh "rm -rf ${LPR_DIR}/lib" 
                    }
                }
            }
        }

        stage('Build & Test Linux Binaries') {
            matrix {
                axes {
                    axis { name 'ARCH'; values 'amd64', 'arm64' }
                }
                // IMPORTANT: Ensure you have Jenkins agents configured with the labels:
                // - 'linux-amd64': A Linux agent with FPC installed (and optionally cross-compilers if needed).
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

                            // Use 'dir' to change into the workspace root
                            dir('.') { 
                                echo "Building ${PROJECT_NAME_KAYTE} for ${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG} using FPC..."
                                // CHANGED TO .lpi
                                sh "fpc ${LPR_DIR}/${PROJECT_NAME_KAYTE}.lpi -B -O3 -T${FPC_OS_FLAG} -P${FPC_CPU_FLAG} -FE${kayte_fpc_out_dir} -FU${kayte_fpc_out_dir} -I. -I./source -I./src"
                                sh "mv ${kayte_fpc_out_dir}/${PROJECT_NAME_KAYTE} ${OUTPUT_BIN_DIR}/${CURRENT_MAIN_APP_NAME}"
                                sh "chmod +x ${OUTPUT_BIN_DIR}/${CURRENT_MAIN_APP_NAME}"
                                echo "${PROJECT_NAME_KAYTE} build completed."

                                echo "Building ${PROJECT_NAME_VB6_INTERPRETER} for ${env.FPC_OS_FLAG}-${env.FPC_CPU_FLAG} using FPC..."
                                // CHANGED TO .lpi
                                sh "fpc ${LPR_DIR}/${PROJECT_NAME_VB6_INTERPRETER}.lpi -B -O3 -T${FPC_OS_FLAG} -P${FPC_CPU_FLAG} -FE${vb6_fpc_out_dir} -FU${vb6_fpc_out_dir} -I. -I./source -I./src"
                                sh "mv ${vb6_fpc_out_dir}/${PROJECT_NAME_VB6_INTERPRETER} ${OUTPUT_BIN_DIR}/${CURRENT_INTERPRETER_APP_NAME}"
                                sh "chmod +x ${OUTPUT_BIN_DIR}/${CURRENT_INTERPRETER_APP_NAME}"
                                echo "${PROJECT_NAME_VB6_INTERPRETER} build completed."
                            } // End of dir('.') block
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
                            // Ensure the test script is run from the workspace root or handles paths correctly
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