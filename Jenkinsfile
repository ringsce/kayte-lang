// Jenkinsfile for the Kayte Project

pipeline {
    // Agent: Define where the pipeline will run.
    // 'any' runs on any available agent. For dedicated environments, use a label.
    agent { label 'linux-fpc' } // Example: Assuming you have an agent with 'linux-fpc' label and FPC/Make installed

    // Tools: Automatically install tools configured in Jenkins' 'Global Tool Configuration'.
    // Ensure you have tools named 'DefaultMake' and 'FPC_Compiler_Latest' (or similar) set up in Jenkins.
    tools {
        make 'DefaultMake'        // Name of your Make installation in Jenkins
        fpc 'FPC_Compiler_Latest' // Name of your Free Pascal installation in Jenkins
    }

    // Environment variables: Define variables accessible throughout the pipeline.
    // Useful for build flags, paths, etc.
    environment {
        // Example: Define the name of your main executable for clarity and reusability
        APP_NAME = 'KayteApp'
        // Example: If you need to specify a Free Pascal target (adjust as needed)
        // FPC_TARGET = 'x86_64-linux'
    }

    stages {
        stage('Checkout Source Code') {
            steps {
                echo 'Checking out source code...'
                // Fetch code from your Git repository.
                // Replace placeholders with your actual Git details.
                git branch: 'main', credentialsId: 'your-git-credentials-id', url: 'https://github.com/your-org/kayte.git'
                echo 'Source code checked out.'
            }
        }

        stage('Build Project') {
            steps {
                script {
                    echo 'Starting project build with Make...'
                    // Use 'dir' step if your Makefile is in a subdirectory (e.g., 'src' or 'build')
                    // dir('src') { // Uncomment and adjust if your Makefile is not in the repository root
                        sh 'make all' // Executes the 'all' target in your Makefile
                    // }
                    echo 'Project build complete.'
                }
            }
        }

        stage('Run Tests') {
            // Optional: Add this stage if you have unit tests for your Free Pascal project (e.g., using FPCUnit).
            // Modify 'make test' or './YourTestsApp' based on how you run your tests.
            when {
                expression { return fileExists('Makefile') && sh(returnStatus: true, script: 'grep -q "^test:" Makefile') == 0 }
            }
            steps {
                script {
                    echo 'Running tests...'
                    sh 'make test' // Assuming a 'test' target in your Makefile
                    // Or, if you have a dedicated test executable:
                    // sh './bin/YourTestsApp'
                    echo 'Tests finished.'
                }
            }
            post {
                // Publish test results if your test runner generates reports (e.g., JUnit XML)
                always {
                    // Assuming your tests output JUnit XML reports in 'test-results/' directory
                    // junit 'test-results/**/*.xml'
                }
            }
        }

        stage('Execute Main Application') {
            steps {
                script {
                    echo "Executing main application: ${APP_NAME}..."
                    // Run your compiled application. Adjust path if not in root.
                    sh "./${APP_NAME}"
                    echo 'Main application execution finished.'
                }
            }
        }

        stage('Clean Build Artifacts') {
            steps {
                script {
                    echo 'Cleaning build artifacts...'
                    sh 'make clean' // Executes the 'clean' target in your Makefile
                    echo 'Build artifacts cleaned.'
                }
            }
        }
    }

    post {
        // Post-build actions: These steps always execute at the end of the pipeline, regardless of stage success/failure.
        always {
            echo 'Build pipeline finished.'
            // Clean the entire Jenkins workspace (more aggressive than 'make clean')
            cleanWs()
        }
        success {
            echo 'Build successful! Archiving artifacts...'
            // Archive the main executable and any other important build outputs.
            // Adjust the pattern to match where your compiled binaries are located.
            archiveArtifacts artifacts: "${APP_NAME}" // Archives the main application executable
            // archiveArtifacts artifacts: 'bin/**/*, logs/**/*.log' // Example for multiple artifacts
        }
        failure {
            echo 'Build failed! Sending notification...'
            // Add notification steps here (e.g., email, Slack, Teams)
            // mail to: 'your-email@example.com',
            //      subject: "Kayte Build Failed: ${env.JOB_NAME} #${env.BUILD_NUMBER}",
            //      body: "The Kayte CI build has failed. Check console output: ${env.BUILD_URL}"
        }
        unstable {
            echo 'Build was unstable (e.g., tests passed but static analysis warnings occurred)!'
        }
        // aborted {
        //     echo 'Build was aborted!'
        // }
    }
}