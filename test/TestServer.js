var http = require('http');
var url = require('url');
http.createServer(function (req, res)
{
    res.writeHead(200, {'Content-Type': 'text/plain'});
    var url_parts = url.parse(req.url, true);
    var query = url_parts.query;
    if('/login' === url_parts.pathname)
    {
        if(query.username === 'foo' && query.password === 'bar')
        {
            res.end('8379283747838');
        }
        else
        {
            res.end('failed');
        }
    }
    else if('/listcards')
    {
        var obj = {};
        obj.cards = [ { ID: 0, name : 'cardA'}, {ID:1, name: 'cardB'}],
        obj.deckType = 'generic';
        res.end(JSON.stringify(obj));
    }
}).listen(1337, '127.0.0.1');

console.log('Server running at http://127.0.0.1:1337/');
