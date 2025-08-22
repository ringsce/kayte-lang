// app.js
const express = require('express');
const path = require('path');
const fs = require('fs');

const app = express();
const PORT = process.env.PORT || 3000;

// Middleware to serve static files from the 'public' directory.
// This handles requests for .css, .js, images, and other static assets.
app.use(express.static(path.join(__dirname, 'public')));

// A simple function to simulate parsing a .kayte file.
// In a real application, you would replace this with a more
// complex parsing library or logic.
function parseKayteFile(filePath) {
  try {
    const kayteContent = fs.readFileSync(filePath, 'utf-8');
    // For this example, we'll assume the .kayte file is just HTML.
    // In a real-world scenario, you would transform the content here.
    return kayteContent;
  } catch (error) {
    console.error(`Error parsing .kayte file: ${error.message}`);
    return null;
  }
}

// Handler for all GET requests.
app.get('/', (req, res) => {
  let filePath = path.join(__dirname, 'public', req.path);

  // If the request URI is '/', default to 'index.kayte'.
  if (req.path === '/') {
    filePath = path.join(__dirname, 'public', 'index.kayte');
  }

  const fileExtension = path.extname(filePath).toLowerCase();

  // Check if the file exists.
  if (fs.existsSync(filePath)) {
    if (fileExtension === '.kayte') {
      // Handle .kayte files specifically.
      const parsedContent = parseKayteFile(filePath);
      if (parsedContent !== null) {
        res.setHeader('Content-Type', 'text/html');
        res.send(parsedContent);
      } else {
        res.status(500).send('Error processing .kayte file.');
      }
    } else {
      // For other files, Express's `static` middleware already handles them.
      // This part is for demonstration, but the `express.static` line above
      // already makes it work for most file types.
    }
  } else {
    // If the file doesn't exist, send a 404 response.
    res.status(404).send('404 Not Found');
  }
});

app.listen(PORT, () => {
  console.log(`Server is running at http://localhost:${PORT}`);
});

