// Jenkinsfile for the Kayte Project

pipeline {
    agent any // Or specify a label: agent { label 'your-build-agent' }

    tools {
        // If you have specific versions of Make or Free Pascal configured in Jenkins global tools
        // make 'Make' // Replace 'Make' with the name configured in Jenkins
        // fpc 'FreePascal' // Replace 'FreePascal' with the name configured in Jenkins
    }

    stages {
        stage('Checkout') {
            steps {
                // Assuming your source code is in a Git repository
                git branch: 'main', credentialsId: 'your-git-credentials-id', url: 'your-git-repo-url'
                // Replace 'main' with your default branch, 'your-git-credentials-id' with your Jenkins credentials ID,
                // and 'your-git-repo-url' with the actual URL of your Git repository.
            }
        }

        stage('Build All Projects') {
            steps {
                script {
                    // Make sure the project directories are accessible
                    // Change directory if your Jenkins workspace is not the root of your project
                    // For example, if your Makefile is in a 'build' subdirectory:
                    // dir('path/to/your/makefile/directory') {
                    sh 'make all'
                    // }
                }
            }
        }

        stage('Run Main Application') {
            steps {
                script {
                    // Assuming KayteApp is built in the root directory (as per your Makefile)
                    sh './KayteApp'
                }
            }
        }

        stage('Clean Workspace') {
            steps {
                script {
                    sh 'make clean'
                }
            }
        }
    }

    post {
        // Post-build actions (always executed)
        always {
            echo 'Build pipeline finished.'
        }
        success {
            echo 'Build successful!'
            // You might add archiving artifacts here
            // archiveArtifacts artifacts: 'bin/**/*, build/**/*, KayteApp'
        }
        failure {
            echo 'Build failed!'
        }
        unstable {
            echo 'Build was unstable!'
        }
    }
}
