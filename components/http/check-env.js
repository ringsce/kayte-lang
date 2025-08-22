// check-env.js
const { execSync } = require('child_process');

/**
 * Checks if a command-line tool is installed.
 * @param {string} command The command to check (e.g., 'node', 'npm').
 * @returns {boolean} True if the command is found, false otherwise.
 */
function isToolInstalled(command) {
  try {
    execSync(`${command} -v`, { stdio: 'ignore' });
    return true;
  } catch (error) {
    return false;
  }
}

/**
 * Detects if Node.js is installed on the system.
 */
function checkNodeJs() {
  const isNodeInstalled = isToolInstalled('node');
  if (isNodeInstalled) {
    console.log('✅ Node.js is installed.');
  } else {
    console.error('❌ Node.js is not installed. Please install it to run the server.');
  }
  return isNodeInstalled;
}

/**
 * Detects if npm (Node Package Manager) is installed on the system.
 */
function checkNpm() {
  const isNpmInstalled = isToolInstalled('npm');
  if (isNpmInstalled) {
    console.log('✅ npm is installed.');
  } else {
    console.error('❌ npm is not installed. It is required to install dependencies.');
  }
  return isNpmInstalled;
}

// Example usage
checkNodeJs();
checkNpm();

