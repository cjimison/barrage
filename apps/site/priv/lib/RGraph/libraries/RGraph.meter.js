    /**
    * o------------------------------------------------------------------------------o
    * | This file is part of the RGraph package - you can learn more at:             |
    * |                                                                              |
    * |                          http://www.rgraph.net                               |
    * |                                                                              |
    * | This package is licensed under the RGraph license. For all kinds of business |
    * | purposes there is a small one-time licensing fee to pay and for non          |
    * | commercial  purposes it is free to use. You can read the full license here:  |
    * |                                                                              |
    * |                      http://www.rgraph.net/license                           |
    * o------------------------------------------------------------------------------o
    */
    
    if (typeof(RGraph) == 'undefined') RGraph = {};

    /**
    * The bar chart constructor
    * 
    * @param string canvas The canvas ID
    * @param min    integer The minimum value
    * @param max    integer The maximum value
    * @param value  integer The indicated value
    */
    RGraph.Meter = function (id, min, max, value)
    {
        // Get the canvas and context objects
        this.id                = id;
        this.canvas            = document.getElementById(typeof id === 'object' ? id.id : id);
        this.context           = this.canvas.getContext ? this.canvas.getContext("2d") : null;
        this.canvas.__object__ = this;
        this.type              = 'meter';
        this.min               = min;
        this.max               = max;
        this.value             = value;
        this.centerx           = null;
        this.centery           = null;
        this.radius            = null;
        this.isRGraph          = true;
        this.currentValue      = null;
        this.uid               = RGraph.CreateUID();
        this.canvas.uid        = this.canvas.uid ? this.canvas.uid : RGraph.CreateUID();
        this.colorsParsed      = false;
        this.coordsText        = [];


        /**
        * Compatibility with older browsers
        */
        RGraph.OldBrowserCompat(this.context);


        // Various config type stuff
        this.properties = {
            'chart.background.color':       'white',
            'chart.gutter.left':            15,
            'chart.gutter.right':           15,
            'chart.gutter.top':             15,
            'chart.gutter.bottom':          15,
            'chart.linewidth':              1,
            'chart.linewidth.segments':     0,
            'chart.strokestyle':            null,
            'chart.border':                 true,
            'chart.border.color':           'black',
            'chart.text.font':              'Arial',
            'chart.text.size':              10,
            'chart.text.color':             'black',
            'chart.value.text.decimals':    0,
            'chart.value.text.units.pre':   '',
            'chart.value.text.units.post':  '',
            'chart.title':                  '',
            'chart.title.background':       null,
            'chart.title.hpos':             null,
            'chart.title.vpos':             null,
            'chart.title.color':            'black',
            'chart.title.bold':             true,
            'chart.title.font':             null,
            'chart.title.x':                null,
            'chart.title.y':                null,
            'chart.title.halign':           null,
            'chart.title.valign':           null,
            'chart.green.start':            ((this.max - this.min) * 0.35) + this.min,
            'chart.green.end':              this.max,
            'chart.green.color':            '#207A20',
            'chart.yellow.start':           ((this.max - this.min) * 0.1) + this.min,
            'chart.yellow.end':             ((this.max - this.min) * 0.35) + this.min,
            'chart.yellow.color':           '#D0AC41',
            'chart.red.start':              this.min,
            'chart.red.end':                ((this.max - this.min) * 0.1) + this.min,
            'chart.red.color':              '#9E1E1E',
            'chart.colors.ranges':          null,
            'chart.units.pre':              '',
            'chart.units.post':             '',
            'chart.contextmenu':            null,
            'chart.zoom.factor':            1.5,
            'chart.zoom.fade.in':           true,
            'chart.zoom.fade.out':          true,
            'chart.zoom.hdir':              'right',
            'chart.zoom.vdir':              'down',
            'chart.zoom.frames':            25,
            'chart.zoom.delay':             16.666,
            'chart.zoom.shadow':            true,
            'chart.zoom.background':        true,
            'chart.zoom.action':            'zoom',
            'chart.annotatable':            false,
            'chart.annotate.color':         'black',
            'chart.shadow':                 false,
            'chart.shadow.color':           'rgba(0,0,0,0.5)',
            'chart.shadow.blur':            3,
            'chart.shadow.offsetx':         3,
            'chart.shadow.offsety':         3,
            'chart.resizable':              false,
            'chart.resize.handle.adjust':   [0,0],
            'chart.resize.handle.background': null,
            'chart.tickmarks.small.num':      100,
            'chart.tickmarks.big.num':        10,
            'chart.tickmarks.small.color':    '#bbb',
            'chart.tickmarks.big.color':      'black',
            'chart.scale.decimals':           0,
            'chart.scale.point':              '.',
            'chart.scale.thousand':           ',',
            'chart.radius':                   null,
            'chart.centerx':                  null,
            'chart.centery':                  null,
            'chart.labels':                   true,
            'chart.segment.radius.start':     0,
            'chart.needle.radius':            null,
            'chart.needle.tail':              false,
            'chart.adjustable':               false,
            'chart.angles.start':             PI,
            'chart.angles.end':               TWOPI
        }


        // Check for support
        if (!this.canvas) {
            alert('[METER] No canvas support');
            return;
        }



        /*
        * Translate half a pixel for antialiasing purposes - but only if it hasn't beeen
        * done already
        */
        if (!this.canvas.__rgraph_aa_translated__) {
            this.context.translate(0.5,0.5);
            
            this.canvas.__rgraph_aa_translated__ = true;
        }




        ///////////////////////////////// SHORT PROPERTIES /////////////////////////////////




        var RG   = RGraph;
        var ca   = this.canvas;
        var co   = ca.getContext('2d');
        var prop = this.properties;




        //////////////////////////////////// METHODS ///////////////////////////////////////


    
        /**
        * A setter
        * 
        * @param name  string The name of the property to set
        * @param value mixed  The value of the property
        */
        this.Set = function (name, value)
        {
            name = name.toLowerCase();
    
            /**
            * This should be done first - prepend the propertyy name with "chart." if necessary
            */
            if (name.substr(0,6) != 'chart.') {
                name = 'chart.' + name;
            }
    
            if (name == 'chart.value') {
                this.value = value;
                return;
            }
    
            prop[name] = value;
    
            return this;
        }




        /**
        * A getter
        * 
        * @param name  string The name of the property to get
        */
        this.Get = function (name)
        {
            /**
            * This should be done first - prepend the property name with "chart." if necessary
            */
            if (name.substr(0,6) != 'chart.') {
                name = 'chart.' + name;
            }
    
            if (name == 'chart.value') {
                return this.value;
            }
    
            return prop[name];
        }




        /**
        * The function you call to draw the bar chart
        */
        this.Draw = function ()
        {
            /**
            * Fire the onbeforedraw event
            */
            RG.FireCustomEvent(this, 'onbeforedraw');
    
            /**
            * Constrain the value to be within the min and max
            */
            if (this.value > this.max) this.value = this.max;
            if (this.value < this.min) this.value = this.min;
    
            /**
            * Set the current value
            */
            this.currentValue = this.value;
    
            /**
            * This is new in May 2011 and facilitates indiviual gutter settings,
            * eg chart.gutter.left
            */
            this.gutterLeft   = prop['chart.gutter.left'];
            this.gutterRight  = prop['chart.gutter.right'];
            this.gutterTop    = prop['chart.gutter.top'];
            this.gutterBottom = prop['chart.gutter.bottom'];
            
            this.centerx = ((ca.width - this.gutterLeft - this.gutterRight) / 2) + this.gutterLeft;
            this.centery = ca.height - this.gutterBottom;
            this.radius  = Math.min(
                                    (ca.width - this.gutterLeft - this.gutterRight) / 2,
                                    (ca.height - this.gutterTop - this.gutterBottom)
                                   );
    
    
    
            /**
            * Custom centerx, centery and radius
            */
            if (typeof(prop['chart.centerx']) == 'number') this.centerx = prop['chart.centerx'];
            if (typeof(prop['chart.centery']) == 'number') this.centery = prop['chart.centery'];
            if (typeof(prop['chart.radius']) == 'number')  this.radius  = prop['chart.radius'];
    
    
            /**
            * Parse the colors for gradients. Its down here so that the center X/Y can be used
            */
            if (!this.colorsParsed) {
    
                this.parseColors();
    
                // Don't want to do this again
                this.colorsParsed = true;
            }
    
    
            this.DrawBackground();
            this.DrawNeedle();
            this.DrawLabels();
            this.DrawReadout();
            
            /**
            * Draw the title
            */
            RG.DrawTitle(this, prop['chart.title'], this.gutterTop, null, prop['chart.title.size'] ? prop['chart.title.size'] : prop['chart.text.size'] + 2);
    
            /**
            * Setup the context menu if required
            */
            if (prop['chart.contextmenu']) {
                RG.ShowContext(this);
            }
    
            
            /**
            * This function enables resizing
            */
            if (prop['chart.resizable']) {
                RG.AllowResizing(this);
            }
    
    
            /**
            * This installs the event listeners
            */
            RG.InstallEventListeners(this);



            /**
            * Fire the RGraph ondraw event
            */
            RG.FireCustomEvent(this, 'ondraw');



            return this;
        }




        /**
        * Draws the background of the chart
        */
        this.DrawBackground = function ()
        {
            /**
            * Draw the white background
            */
            co.beginPath();
    
                co.fillStyle = prop['chart.background.color'];
                
                if (prop['chart.shadow']) {
                    RG.SetShadow(this, prop['chart.shadow.color'],prop['chart.shadow.offsetx'],prop['chart.shadow.offsety'], prop['chart.shadow.blur']);
                }
                co.moveTo(this.centerx,this.centery);
                co.arc(this.centerx,
                       this.centery,
                       this.radius,
                       prop['chart.angles.start'],
                       prop['chart.angles.end'],
                       false);
    
            co.fill();
            
            RG.NoShadow(this);
    
            
            // Draw the shadow
            if (prop['chart.shadow']) {
    
                co.beginPath();
                    var r = (this.radius * 0.06) > 40 ? 40 : (this.radius * 0.06);
                    co.arc(this.centerx, this.centery, r, 0, TWOPI, 0);
                co.fill();
    
                RG.NoShadow(this);
            }



            // First, draw the grey tickmarks
            if (prop['chart.tickmarks.small.num']) {
                for (var i=0; i<(prop['chart.angles.end'] - prop['chart.angles.start']); i+=(PI / prop['chart.tickmarks.small.num'])) {
                    co.beginPath();
                        co.strokeStyle = prop['chart.tickmarks.small.color'];
                        co.arc(this.centerx, this.centery, this.radius, prop['chart.angles.start'] + i, prop['chart.angles.start'] + i + 0.00001, 0);
                        co.arc(this.centerx, this.centery, this.radius - 5, prop['chart.angles.start'] + i, prop['chart.angles.start'] + i + 0.00001, 0);
                    co.stroke();
                }
    
                // Draw the semi-circle that makes the tickmarks
                co.beginPath();
                    co.fillStyle = prop['chart.background.color'];
                    co.arc(this.centerx, this.centery, this.radius - 4, prop['chart.angles.start'], prop['chart.angles.end'], false);
                co.closePath();
                co.fill();
            }
    
    
            // Second, draw the darker tickmarks. First run draws them in white to get rid of the existing tickmark,
            // then the second run draws them in the requested color
            
            
            if (prop['chart.tickmarks.big.num']) {
                var colors = ['white','white',prop['chart.tickmarks.big.color']];
                for (var j=0; j<colors.length; ++j) {
                    for (var i=0; i<(prop['chart.angles.end'] - prop['chart.angles.start']); i+=((prop['chart.angles.end'] - prop['chart.angles.start']) / prop['chart.tickmarks.big.num'])) {
                        co.beginPath();
                            co.strokeStyle = colors[j];
                            co.arc(this.centerx, this.centery, this.radius, prop['chart.angles.start'] +  i, prop['chart.angles.start'] + i + 0.001, 0);
                            co.arc(this.centerx, this.centery, this.radius - 5, prop['chart.angles.start'] + i, prop['chart.angles.start'] + i + 0.0001, 0);
                        co.stroke();
                    }
                }
            }
    
            // Draw the white circle that makes the tickmarks
            co.beginPath();
            co.fillStyle = prop['chart.background.color'];
            co.moveTo(this.centerx, this.centery);
            co.arc(this.centerx, this.centery, this.radius - 7, prop['chart.angles.start'], prop['chart.angles.end'], false);
            co.closePath();
            co.fill();
    
            /**
            * Color ranges - either green/yellow/red or an arbitrary number of ranges
            */
            var ranges = prop['chart.colors.ranges'];
    
            if (RG.is_array(prop['chart.colors.ranges'])) {
    
                var ranges = prop['chart.colors.ranges'];
    
                for (var i=0; i<ranges.length; ++i) {
    
                    co.strokeStyle = prop['chart.strokestyle'] ? prop['chart.strokestyle'] : ranges[i][2];
                    co.fillStyle = ranges[i][2];
                    co.lineWidth = prop['chart.linewidth.segments'];
    
                    co.beginPath();
                        co.arc(this.centerx,
                               this.centery,
                               this.radius * 0.85,
                               (((ranges[i][0] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                               (((ranges[i][1] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                               false);
    
                        if (prop['chart.segment.radius.start'] > 0) {
                            co.arc(this.centerx,
                                   this.centery,
                                   prop['chart.segment.radius.start'],
                                   (((ranges[i][1] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                                   (((ranges[i][0] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                                   true);
                        } else {
                            co.lineTo(this.centerx, this.centery);
                        }
    
                    co.closePath();
                    co.stroke();
                    co.fill();
                }
    
                // Stops the last line from being changed to a big linewidth.
                co.beginPath();
    
            } else {
                co.lineWidth = prop['chart.linewidth'];
    
                // Draw the green area
                co.strokeStyle = prop['chart.strokestyle'] ? prop['chart.strokestyle'] : prop['chart.green.color'];
                co.fillStyle   = prop['chart.green.color'];
                co.lineWidth   = prop['chart.linewidth.segments'];
                
                co.beginPath();
                    co.arc(this.centerx,
                           this.centery,
                           this.radius * 0.85,
                           (((prop['chart.green.start'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - this.properties['chart.angles.start'])) + prop['chart.angles.start'],
                           (((prop['chart.green.end'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                           false);
    
                    if (prop['chart.segment.radius.start'] > 0) {
    
                        co.arc(this.centerx,
                               this.centery,
                               prop['chart.segment.radius.start'],
                               (((prop['chart.green.end'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                               (((prop['chart.green.start'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                               true);
                    } else {
                        co.lineTo(this.centerx, this.centery);
                    }
    
                co.closePath();
                co.stroke();
                co.fill();
                
                // Draw the yellow area
                co.strokeStyle = prop['chart.strokestyle'] ? prop['chart.strokestyle'] : prop['chart.yellow.color'];
                co.fillStyle = prop['chart.yellow.color'];
                co.lineWidth = prop['chart.linewidth.segments'];
                co.beginPath();
                co.arc(this.centerx,
                       this.centery,
                       this.radius * 0.85,
                       (((prop['chart.yellow.start'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                       (((prop['chart.yellow.end'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                       false);
                
                if (prop['chart.segment.radius.start'] > 0) {
                    co.arc(this.centerx,
                           this.centery,
                           prop['chart.segment.radius.start'],
                           (((prop['chart.yellow.end'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                           (((prop['chart.yellow.start'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                           true);
                } else {
                    co.lineTo(this.centerx, this.centery);
                }
    
                co.closePath();
                co.stroke();
                co.fill();
                
                // Draw the red area
                co.strokeStyle = prop['chart.strokestyle'] ? prop['chart.strokestyle'] : prop['chart.red.color'];
                co.fillStyle = prop['chart.red.color'];
                co.lineWidth = prop['chart.linewidth.segments'];
                
                co.beginPath();
                    co.arc(this.centerx,
                           this.centery,this.radius * 0.85,
                           (((prop['chart.red.start'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                           (((prop['chart.red.end'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                           false);
        
                    if (prop['chart.segment.radius.start'] > 0) {
                        co.arc(this.centerx,
                               this.centery,
                               prop['chart.segment.radius.start'],
                               (((prop['chart.red.end'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                               (((prop['chart.red.start'] - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'],
                               true);
                    } else {
                        co.lineTo(this.centerx, this.centery);
                    }
    
                co.closePath();
                co.stroke();
                co.fill();
                
                // Revert the linewidth
                co.lineWidth = 1;
            }
    
            // Draw the outline
            if (prop['chart.border']) {
                co.strokeStyle = prop['chart.border.color'];
                co.lineWidth   = prop['chart.linewidth'];
                
                co.beginPath();
                    co.moveTo(this.centerx, this.centery);
                    co.arc(this.centerx,
                           this.centery,
                           this.radius,
                           prop['chart.angles.start'],
                           prop['chart.angles.end'],
                           false);
                co.closePath();
            }
    
            co.stroke();
            
            // Reset the linewidth back to 1
            co.lineWidth = 1;
        }




        /**
        * Draws the pointer
        */
        this.DrawNeedle = function ()
        {
            // Allow customising the needle radius
            var needleRadius = typeof(prop['chart.needle.radius']) == 'number' ? prop['chart.needle.radius'] : this.radius * 0.7;
    
            // First draw the circle at the bottom
            co.fillStyle = 'black';
            co.lineWidth = this.radius >= 200 ? 7 : 3;
            co.lineCap = 'round';
    
            // Now, draw the arm of the needle
            co.beginPath();
                co.strokeStyle = 'black';
                if (typeof(prop['chart.needle.linewidth']) == 'number') co.lineWidth = prop['chart.needle.linewidth'];
                
                var a = (((this.value - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'];
    
                co.arc(this.centerx, this.centery, needleRadius, a, a + 0.001, false);
                co.lineTo(this.centerx, this.centery);
            co.stroke();

            // Draw the triangular needle head
            co.beginPath();
                co.lineWidth = 1;
                //co.moveTo(this.centerx, this.centery);
                co.arc(this.centerx, this.centery, needleRadius + 15, a, a + 0.001, 0);
                co.arc(this.centerx, this.centery, needleRadius - 15, a + 0.087, a + 0.087999, 0);
                co.arc(this.centerx, this.centery, needleRadius - 15, a - 0.087, a - 0.087999, 1);
            co.fill();

            // Draw the tail if requested
            if (prop['chart.needle.tail']) {
                co.beginPath();
                    co.strokeStyle = 'black';
                    if (typeof(prop['chart.needle.linewidth']) == 'number') co.lineWidth = prop['chart.needle.linewidth'];

                    var a = ((this.value - this.min) / (this.max - this.min) * (this.properties['chart.angles.end'] - this.properties['chart.angles.start'])) + this.properties['chart.angles.start'] + PI;
                    co.arc(this.centerx, this.centery, 25, a, a + 0.001, false);
                    co.lineTo(this.centerx, this.centery);
                co.stroke();
            }

            // Draw the center circle
            var r = (this.radius * 0.06) > 40 ? 40 : (this.radius * 0.06);
    
            co.beginPath();
            co.arc(this.centerx, this.centery, r, 0 + 0.001, TWOPI, 0);
            co.fill();














            // Draw the centre bit of the circle
            co.fillStyle = 'white';
            co.beginPath();
            co.arc(this.centerx, this.centery, r - 2, 0 + 0.001, TWOPI, 0);
            co.fill();
        }




        /**
        * Draws the labels
        */
        this.DrawLabels = function ()
        {
            if (!prop['chart.labels']) {
                return;
            }

            var radius     = this.radius;
            var text_size  = prop['chart.text.size'];
            var text_font  = prop['chart.text.font'];
            var units_post = prop['chart.units.post'];
            var units_pre  = prop['chart.units.pre'];
            var centerx    = this.centerx;
            var centery    = this.centery;
            var min        = this.min;
            var max        = this.max;
            var decimals   = prop['chart.scale.decimals'];
    
            co.fillStyle = prop['chart.text.color'];
            co.lineWidth = 1;
    
            co.beginPath();
    
            for (var i=0; i<=10; ++i) {
            
                var angle      = ((prop['chart.angles.end'] - prop['chart.angles.start']) * (i / 10)) + prop['chart.angles.start'];
                var coords     = RG.getRadiusEndPoint(centerx, centery, angle + (((i==0||i==10) && prop['chart.border']) ? (i==0 ? 0.05 : -0.05) : 0), this.radius * 0.925);
                
                var angleStart = prop['chart.angles.start'];
                var angleEnd   = prop['chart.angles.end'];
                var angleRange = angleEnd - angleStart;
                
                var angleStart_degrees = angleStart * (180 / PI);
                var angleEnd_degrees = angleEnd * (180 / PI);
                var angleRange_degrees = angleRange * (180 / PI);
                
                // Vertical alignment
                valign = 'center';
    
                // Horizontal alignment
                if (prop['chart.border']) {
                    if (i == 0) {
                        halign = 'left';
                    } else if (i == 10) {
                        halign = 'right';
                    } else {
                        halign = 'center'
                    }
                } else {
                    halign = 'center';
                }
    
                RG.Text2(this, {'font':text_font,
                                'size':text_size,
                                'x':coords[0],
                                'y':coords[1],
                                'text':RG.number_format(this, (((this.max - this.min) * (i / 10)) + this.min).toFixed(decimals),units_pre,units_post),
                                'halign':halign,
                                'valign':valign,
                                'angle':((angleRange_degrees * 0.1 * i) + angleStart_degrees) - 270,
                                'bounding':false,
                                'boundingFill':(i == 0 || i == 10) ? 'white': null,
                                'tag': 'scale'
                               });
            }
        }




        /**
        * This function draws the text readout if specified
        */
        this.DrawReadout  = function ()
        {
            if (prop['chart.value.text']) {
                co.beginPath();
                RG.Text2(this, {'font':prop['chart.text.font'],
                                'size':prop['chart.text.size'],
                                'x':this.centerx,
                                'y':this.centery - prop['chart.text.size'] - 15,
                                'text':prop['chart.value.text.units.pre'] + (this.value).toFixed(prop['chart.value.text.decimals']) + prop['chart.value.text.units.post'],
                                'halign':'center',
                                'valign':'bottom',
                                'bounding':true,
                                'boundingFill':'white',
                                'tag': 'value.text'
                               });
    
                co.stroke();
                co.fill();
            }
        }




        /**
        * A placeholder function
        * 
        * @param object The event object
        */
        this.getShape = function (e)
        {
        }




        /**
        * This function returns the pertinent value for a particular click (or other mouse event)
        * 
        * @param obj e The event object
        */
        this.getValue = function (e)
        {
            var mouseXY = RG.getMouseXY(e);
            var angle   = RG.getAngleByXY(this.centerx, this.centery, mouseXY[0], mouseXY[1]);

            // Work out the radius
            var radius = RG.getHypLength(this.centerx, this.centery, mouseXY[0], mouseXY[1]);
            if (radius > this.radius) {
                return null;
            }
    
    
            if (angle < HALFPI) {
                angle += TWOPI;
            }

            var value = (((angle - prop['chart.angles.start']) / (prop['chart.angles.end'] - prop['chart.angles.start'])) * (this.max - this.min)) + this.min;

            value = Math.max(value, this.min);
            value = Math.min(value, this.max);

            return value;
        }




        /**
        * The getObjectByXY() worker method. Don't call this call:
        * 
        * RGraph.ObjectRegistry.getObjectByXY(e)
        * 
        * @param object e The event object
        */
        this.getObjectByXY = function (e)
        {
            var mouseXY = RGraph.getMouseXY(e);
    
            // Work out the radius
            var radius = RG.getHypLength(this.centerx, this.centery, mouseXY[0], mouseXY[1]);
    
            if (
                   mouseXY[0] > (this.centerx - this.radius)
                && mouseXY[0] < (this.centerx + this.radius)
                && mouseXY[1] > (this.centery - this.radius)
                && mouseXY[1] < (this.centery + this.radius)
                && radius <= this.radius
                ) {
    
                return this;
            }
        }




        /**
        * This method handles the adjusting calculation for when the mouse is moved
        * 
        * @param object e The event object
        */
        this.Adjusting_mousemove = function (e)
        {
            /**
            * Handle adjusting for the Bar
            */
            if (prop['chart.adjustable'] && RG.Registry.Get('chart.adjusting') && RG.Registry.Get('chart.adjusting').uid == this.uid) {
                this.value = this.getValue(e);
                RG.Clear(this.canvas);
                RG.RedrawCanvas(this.canvas);
                RG.FireCustomEvent(this, 'onadjust');
            }
        }




        /**
        * This method returns the appropriate angle for a value
        * 
        * @param number value The value
        */
        this.getAngle = function (value)
        {
            // Higher than max
            if (value > this.max || value < this.min) {
                return null;
            }
    
            var angle = (((value - this.min) / (this.max - this.min)) * (prop['chart.angles.end'] - prop['chart.angles.start'])) + prop['chart.angles.start'];
    
            return angle;
        }




        /**
        * This allows for easy specification of gradients
        */
        this.parseColors = function ()
        {
            // Parse the basic colors
            prop['chart.green.color']  = this.parseSingleColorForGradient(prop['chart.green.color']);
            prop['chart.yellow.color'] = this.parseSingleColorForGradient(prop['chart.yellow.color']);
            prop['chart.red.color']    = this.parseSingleColorForGradient(prop['chart.red.color']);
    
            // Parse chart.colors.ranges
            var ranges = prop['chart.colors.ranges'];
            if (ranges && ranges.length) {
                for (var i=0; i<ranges.length; ++i) {
                    ranges[i][2] = this.parseSingleColorForGradient(ranges[i][2]);
                }
            }
        }




        /**
        * This parses a single color value
        */
        this.parseSingleColorForGradient = function (color)
        {            
            if (!color || typeof(color) != 'string') {
                return color;
            }
    
            if (color.match(/^gradient\((.*)\)$/i)) {
    
                var parts = RegExp.$1.split(':');
    
                // Create the gradient
                var grad = co.createRadialGradient(this.centerx, this.centery, prop['chart.segment.radius.start'], this.centerx, this.centery, this.radius * 0.85);
    
                var diff = 1 / (parts.length - 1);
    
                for (var j=0; j<parts.length; ++j) {
                    grad.addColorStop(j * diff, RG.trim(parts[j]));
                }
            }
    
            return grad ? grad : color;
        }




        /**
        * Register the object
        */
        RG.Register(this);
    }