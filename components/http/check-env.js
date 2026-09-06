const { execSync } = require('child_process');

function isToolInstalled(command) {
  try {
    execSync(`${command} -v`, { stdio: 'ignore' });
    return true;
  } catch (error) {
    return false;
  }
}

function checkNodeJs() {
  const isNodeInstalled = isToolInstalled('node');
  if (isNodeInstalled) {
    console.log('✅ Node.js is installed.');
  } else {
    console.error('❌ Node.js is not installed. Please install it to run the server.');
  }
  return isNodeInstalled;
}

function checkNpm() {
  const isNpmInstalled = isToolInstalled('npm');
  if (isNpmInstalled) {
    console.log('✅ npm is installed.');
  } else {
    console.error('❌ npm is not installed. It is required to install dependencies.');
  }
  return isNpmInstalled;
}

checkNodeJs();
checkNpm();
