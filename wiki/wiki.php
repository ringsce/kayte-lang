<?php
require 'Parsedown.php';

$page = isset($_GET['page']) ? basename($_GET['page']) : 'Home';
$filepath = __DIR__ . "/pages/$page.md";

if (!file_exists($filepath)) {
    http_response_code(404);
    $content = "# 404 Not Found\nPage **$page** does not exist.";
} else {
    $content = file_get_contents($filepath);
}

$Parsedown = new Parsedown();
$html = $Parsedown->text($content);
?>
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Wiki - <?= htmlspecialchars($page) ?></title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <nav>
        <a href="wiki.php?page=Home">Home</a> |
        <a href="wiki.php?page=About">About</a>
        <a href="wiki.php?page=GuetingStarted">Gueting Started</a>
    </nav>
    <main>
        <?= $html ?>
    </main>
</body>
</html>
