const express = require('express');
const path = require('path');
const fs = require('fs');

const app = express();
const PORT = process.env.PORT || 3000;

app.use(express.static(path.join(__dirname, 'public')));

// stub: just reads the file as-is and treats it as HTML, no real .kayte parsing yet
function parseKayteFile(filePath) {
  try {
    const kayteContent = fs.readFileSync(filePath, 'utf-8');
    return kayteContent;
  } catch (error) {
    console.error(`Error parsing .kayte file: ${error.message}`);
    return null;
  }
}

app.get('/', (req, res) => {
  const publicDir = path.join(__dirname, 'public');

  let filePath = req.path === '/'
    ? path.join(publicDir, 'index.kayte')
    : path.join(publicDir, req.path);

  // reject anything that resolves outside publicDir (e.g. "..") -
  // path.join alone won't stop traversal, it just normalizes ".." segments
  const relative = path.relative(publicDir, filePath);
  if (relative.startsWith('..') || path.isAbsolute(relative)) {
    res.status(404).send('404 Not Found');
    return;
  }

  const fileExtension = path.extname(filePath).toLowerCase();

  if (fs.existsSync(filePath)) {
    if (fileExtension === '.kayte') {
      const parsedContent = parseKayteFile(filePath);
      if (parsedContent !== null) {
        res.setHeader('Content-Type', 'text/html');
        res.send(parsedContent);
      } else {
        res.status(500).send('Error processing .kayte file.');
      }
    }
    // anything else was already served by express.static above
  } else {
    res.status(404).send('404 Not Found');
  }
});

app.listen(PORT, () => {
  console.log(`Server is running at http://localhost:${PORT}`);
});

