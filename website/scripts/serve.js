/**
 * Static server of the built website (dist/).
 *
 * The site is a single-page application, so unknown paths return index.html
 * and the client router opens the page.
 *
 * Environment variables:
 *   PORT        - port of the server (default 8080)
 *   HOST        - host of the server (default 0.0.0.0)
 *   PUBLIC_PATH - base path the site was built with (default /)
 */
const fs = require('node:fs');
const http = require('node:http');
const path = require('node:path');
const zlib = require('node:zlib');

const root = path.resolve(__dirname, '../dist');
const port = Number(process.env.PORT || 8080);
const host = process.env.HOST || '0.0.0.0';
const publicPath = `/${(process.env.PUBLIC_PATH || '/').replace(/^\/+|\/+$/g, '')}/`.replace('//', '/');

const mimeTypes = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.ico': 'image/x-icon',
  '.txt': 'text/plain; charset=utf-8',
  '.woff2': 'font/woff2',
};

const compressible = new Set(['.html', '.js', '.css', '.json', '.svg', '.txt']);

if (!fs.existsSync(path.join(root, 'index.html'))) {
  console.error('The website is not built. Run `npm run build` first.');
  process.exit(1);
}

/**
 * Returns the file of the request or null if the file does not exist
 */
const resolveFile = urlPath => {
  if (!urlPath.startsWith(publicPath)) {
    return null;
  }

  const relative = decodeURIComponent(urlPath.slice(publicPath.length));
  const file = path.resolve(root, relative);

  // Paths outside of dist/ (../../etc/passwd) are not served
  if (file !== root && !file.startsWith(`${root}${path.sep}`)) {
    return null;
  }

  return fs.existsSync(file) && fs.statSync(file).isFile() ? file : null;
};

const server = http.createServer((req, res) => {
  if (req.method !== 'GET' && req.method !== 'HEAD') {
    res.writeHead(405, { Allow: 'GET, HEAD' });
    res.end();

    return;
  }

  let urlPath;
  try {
    urlPath = new URL(req.url || '/', 'http://localhost').pathname;
  } catch {
    res.writeHead(400);
    res.end();

    return;
  }

  const file = resolveFile(urlPath) || path.join(root, 'index.html');
  const ext = path.extname(file).toLowerCase();

  // Files with a content hash in the name never change, index.html must always be fresh
  const isHashed = /-[0-9a-f]{16,}\./.test(path.basename(file));
  const headers = {
    'Content-Type': mimeTypes[ext] || 'application/octet-stream',
    'Cache-Control': isHashed ? 'public, max-age=31536000, immutable' : 'no-cache',
    'X-Content-Type-Options': 'nosniff',
  };

  const useGzip = compressible.has(ext) && /\bgzip\b/.test(req.headers['accept-encoding'] || '');
  if (useGzip) {
    headers['Content-Encoding'] = 'gzip';
    headers.Vary = 'Accept-Encoding';
  }

  res.writeHead(200, headers);

  if (req.method === 'HEAD') {
    res.end();

    return;
  }

  const stream = fs.createReadStream(file);
  stream.on('error', () => res.destroy());
  (useGzip ? stream.pipe(zlib.createGzip()) : stream).pipe(res);
});

server.listen(port, host, () => {
  const displayHost = host === '0.0.0.0' ? 'localhost' : host;
  console.log(`Website is available on http://${displayHost}:${port}${publicPath}`);
});
