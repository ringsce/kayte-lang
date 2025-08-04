// Jenkinsfile for KayteLang Interpreter
// This pipeline builds universal macOS binaries and Linux ARM64 binaries
// using a custom shell script.

// Assumes the Jenkins agent is a macOS machine with:
// - Git installed
// - Free Pascal Compiler (FPC) installed (e.g., via Homebrew or fpcupdeluxe)
//   and configured for cross-compilation to Linux ARM64.
// - Xcode Command Line Tools installed (for 'ld' and 'lipo').

pipeline {
    // Define the agent where the pipeline will run.
    // 'any' means Jenkins will pick any available agent.
    // For a specific macOS agent, you might use: agent { label 'macos-build-agent' }
    agent any

    stages {
        // Stage 1: Checkout Source Code
        stage('Checkout') {
            steps {
                echo 'Checking out source code...'
                // Uses the SCM configured for the Jenkins job (e.g., Git repository)
                checkout scm
            }
        }

        // Stage 2: Build Project
        // This stage executes your custom.sh script which handles:
        // - FPC compilation for aarch64 (macOS) and x86_64 (macOS)
        // - FPC cross-compilation for aarch64 (Linux)
        // - Custom linking for each architecture
        // - 'lipo' to create the universal macOS binary
        stage('Build') {
            steps {
                echo 'Starting custom build process...'
                script {
                    // Ensure the custom.sh script is executable
                    sh 'chmod +x custom.sh'

                    // Execute the custom build script
                    // The script's output will appear in the Jenkins console log.
                    sh './custom.sh'
                }
            }
        }

        // Stage 3: Run Tests (Placeholder)
        // This stage is a placeholder for running automated tests.
        // You would add commands here to execute your test suite.
        stage('Test') {
            steps {
                echo 'Running tests (placeholder - implement your test commands here)...'
                // Example: sh './run_unit_tests.sh'
                // Example: sh './run_integration_tests.sh'
            }
        }

        // Stage 4: Archive Build Artifacts
        // This stage archives the compiled executables so they can be downloaded
        // from the Jenkins build page.
        stage('Archive Artifacts') {
            steps {
                echo 'Archiving build artifacts...'
                // Archive the universal macOS binary
                archiveArtifacts artifacts: 'projects/vb6interpreter', fingerprint: true, allowEmpty: false
                // Archive the Linux ARM64 binary
                archiveArtifacts artifacts: 'projects/build_linux_aarch64/vb6interpreter_linux_aarch64', fingerprint: true, allowEmpty: false
            }
        }

        // Stage 5: Deploy (Placeholder)
        // This stage is a placeholder for deploying your application.
        // You would add commands here to copy the binaries to a deployment server,
        // publish them, etc.
        stage('Deploy') {
            steps {
                echo 'Deploying application (placeholder - implement your deployment logic here)...'
                // Example: sh 'scp projects/vb6interpreter user@your_macos_server:/path/to/app/'
                // Example: sh 'scp projects/build_linux_aarch64/vb6interpreter_linux_aarch64 user@your_linux_server:/path/to/app/'
            }
        }
    }

    // Post-build actions: run regardless of stage success/failure
    post {
        always {
            echo 'Pipeline finished.'
        }
        success {
            echo 'Build successful! 🎉'
            // Add any success notifications here (e.g., email, Slack)
        }
        failure {
            echo 'Build failed! 💔'
            // Add any failure notifications here
        }
        unstable {
            echo 'Build was unstable (e.g., tests failed but build passed).'
        }
        aborted {
            echo 'Build was aborted.'
        }
    }
}
