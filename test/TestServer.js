//-------------------------------------------------------------------
// Copyright (c) 2013 Christopher Jimison
//
// Permission is hereby granted, free of charge, to any person obtaining 
// a copy of this software and associated documentation files 
// (the "Software"), to deal in the Software without restriction, 
// including without limitation the rights to use, copy, modify, merge, 
// publish, distribute, sublicense, and/or sell copies of the Software, 
// and to permit persons to whom the Software is furnished to do so, 
// subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be 
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY 
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//-------------------------------------------------------------------

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
            var sesObj = { session: 8379283747838 };

            //res.end(util.inspect(querystring.parse(chunk)));
            res.end(JSON.stringify(sesObj));
        });
    }
}).listen(1337, '0.0.0.0');

console.log('Server running at http://127.0.0.1:1337/');
