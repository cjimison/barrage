Barrage
=======

<OUT OF DATE>

Scalable http load testing framework

# Another http load testing system?  WTF

I agree with you but let me state my case.  Having used systems like Siege and Apache Bench I wanted something that gives me the following things:

1. Easy to model user interaction with my system
2. Something that can scale out so I can just hammer the living crap out of my server

While multiple systems offered some of these, none of them had it to the level that I wanted.  So that is when I came up with Barrage.  

Having a background in game development I really love behavior trees.  They are simple to model and the best thing is when you do them right you don't need to be an engineer to create them.  And when looking at all these different load testing systems I thought to myself "Man, I really wish I could just create a behavior tree to model all this crap out fast and then run it".  So that is how the idea was born.

# Basic config

All barrage configurations are written in JSON

## Actions

First you need to define a bunch of actions.  This is what the system will do as they execute.  A action can basically be thought of as two parts:

1. The http end point you want to hit
2. The data you are sending or receiving and what you want to do with it.

### Example Action

```javascript
[
    {
        "name"          : "login",
        "url"           : "/login",
        "type"          : "get",
        "args"          :
        {
            "username"  : "foo",
            "password"  : "bar"
        },
        "results"       : { "session" : "$SESSION" },
        "args_type"     : "http",
        "results_type"  : "query_str"
    },
    ...
]
```
I will break these down for ya:

1. name = The name of this action.  When you build behaviors this is the text you will refer to it later as
2. url = The full URL of the endpoint you want to hit.  NOTE:  I am working on making this a variable you can set and replace
3. type = The http type of request to send.  Currently support get, post, put
4. args = Array of key/value pairs for the http arguments
5. args_type = how should the arguments be packaged.  Currently I only support http but JSON will be supported soon
6. results = currently it is expected that all data will come back as json (big assumption I know and will change later).  Each "client" process will have a key/value cache assigned to it so this will look for a member called "session" in the returning object and save it as a token called "$SESSION.  In your scripts you can ask the client for this value back by using the key $SESSION.  NOTE:  This is not very clear.  I need o doc this better with a better example.
7. results_type = How should the results be process.  Currently I only support json

### System Actions

These are here to help you build more complex behaviors.  Currently I support the following system actions:

1. <<"ordered">> = This action will execute all it's children in order they are defined in the array
2. <<"random">> = This action will randomly pick one child to execute
3. <<"wait">> = This action will sleep the client for N amount of time
4. <<"random_wait">> = This action will sleep the client for a random amount of time defined a 0-N milliseconds

## Behaviors

This is where the fun comes.  You can build out a really complex set of client interactions by defining a "behavior" to execute.

### Example Behavior

```javascript

{
    "name" : "Simple User",
    "tree" : 
    { 
        "action"    : "ordered",
        "args"      : { "count" : 1 },
        "children"  : [
            {
                "action"    : "load_kv_store",
                "args"      : 
                { 
                    "kv_store"  : "USER_NAMES_DATA", 
                    "file"      : "user_data.json" 
                }
            },
            {
                "action"    : "read_random_kv",
                "args"      : 
                { 
                    "kv_store"  : "USER_NAMES_DATA", 
                    "key"       : "$USERNAME", 
                    "value"     : "$PASSWORD" 
                }
            },
            { "action" : "named_login"},
            { 
                "action" : "random", 
                "args"   : { "count", 10},
                "children": [
                    { "action", "wait", "args" : {"time", 1000}}, 
                    { "action", "random_wait", "args" : {"time", 2000}}, 
                    { 
                        "action" : "random",
                        "children" : [
                            { "action" : "add_card"},
                            { "action" : "get_cards"}
                        ],
                        "args" : {"min_time" : 5000}
                    ],
                    { "action" : "get_cards"}
                ]
            }
        ]
    }
},
```
So you maybe saying to yourself "WTF, I though you side this crap was meant to be easy?!?!".  There is one major feature you are missing:

This is a data file, not code.  After the base system has stabilize and works well the plan is to start working on a slick UI front end like so:

[Example Behavior Tree Editor](http://brainiac.codeplex.com/)


I was thinking about putting something in HTML5 so I can get the same kind of user interaction but it can also be used to display reports and test runs from the server.

# Did I sell ya on it?

If not I understand. I am not trying to convince people who get a lot of mileage out of existing systems to believe this is somehow better for your use cases.

If this does sound cool then please stay tuned.  Barrage is still in the prototype/weekend work phase so don't expect a lot soon...
