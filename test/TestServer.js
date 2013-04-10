var http = require('http');
var url = require('url');
var util=require('util');
var querystring=require('querystring');
var count = 0;
http.createServer(function (req, res)
{
    if(req.method=='GET')
    {
        res.writeHead(200, {'Content-Type': 'text/plain'});
        var url_parts = url.parse(req.url, true);
        var query = url_parts.query;
        var obj = {};
        if('/login' === url_parts.pathname)
        {
            console.log("I got a login request");
            if(query.username === 'foo' && query.password === 'bar')
            {
                res.end('8379283747838');
            }
            else
            {
                res.end('failed');
            }
        }
        else if('/listcards' === url_parts.pathname)
        {
            console.log("I got a listcards request");
            obj.cards = [ { ID: 0, name : 'cardA'}, {ID:1, name: 'cardB'}];
            obj.deckType = 'generic';
            res.end(JSON.stringify(obj));
        }
        else if('/get_card_details' === url_parts.pathname)
        {
            console.log("I got a get_card_details request");
            obj.card = { ID : 2, name : 'cardC'};
            res.end(JSON.stringify(obj));
        }
        else if('/add_card' === url_parts.pathname)
        {
            console.log("I got a add_card request");
            obj.card = { ID : 3, name : 'cardD'};
            res.end(JSON.stringify(obj));
        }
        else if('/cookie' === url_parts.pathname)
        {
            console.log("Cookies = " + req.headers.cookie);
            res.writeHead(200, { 
                'Set-Cookie': 'mycookie=test'+count,
                'Content-Type': 'text/plain'
            });
            count++;
            obj.card = { ID : 4, name : 'cookie'};
            res.end(JSON.stringify(obj));
        }
        else if('/cookie2' === url_parts.pathname)
        {
            console.log("Cookies2 = " + req.headers.cookie);
            res.writeHead(200, { 
                'Set-Cookie': 'mycookie=test'+count,
                'Content-Type': 'text/plain'
            });
            count++;
            obj.card = { ID : 5, name : 'cookie2'};
            res.end(JSON.stringify(obj));
        }
    }
    else
    {
        console.log("Got post request");
        var chunk = '';
        req.on('data', function (data) {
            chunk += data;
        });
        req.on('end', function () {
            var rez = querystring.parse(chunk);
            console.log("Username = " + rez.username + ", Password = " + rez.password);
            res.end(util.inspect(querystring.parse(chunk)));
        });
    }
}).listen(1337, '0.0.0.0');

console.log('Server running at http://127.0.0.1:1337/');
