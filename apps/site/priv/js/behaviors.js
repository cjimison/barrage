// Behavior editor

// Function init
// This should pull the current behavior tree info from server
function init(){
    dimension(800, 1000);
    
    var fsa = Joint.dia.fsa;
    Joint.paper("world", 800, 1000);

    var s1 = fsa.State.create({
      position: {x: 120, y: 70},
      label: "state 1"
    });
    var s2 = fsa.State.create({
      position: {x: 250, y: 100},
      label: "state 2"
    });
    var s3 = fsa.State.create({
      position: {x: 150, y: 200},
      label: "state 3"
    });
    var s4 = fsa.State.create({
      position: {x: 350, y: 180},
      label: "state 4"
    });
    var s5 = fsa.State.create({
      position: {x: 180, y: 300},
      label: "state 5"
    });
    var s6 = fsa.State.create({
      position: {x: 300, y: 300},
      label: "state 6"
    });
    var s0 = fsa.StartState.create({
      position: {x: 20, y: 20}
    });
    var se = fsa.EndState.create({
      position: {x: 350, y: 50}
    });

    var all = [s0, se, s1, s2, s3, s4, s5, s6];

    s0.joint(s1, fsa.arrow).register(all);
    s1.joint(s2, fsa.arrow).register(all);
    s1.joint(s3, fsa.arrow).register(all);
    s2.joint(se, fsa.arrow).register(all);
    s3.joint(s2, fsa.arrow).register(all);
    s3.joint(s5, fsa.arrow).register(all);
    s5.joint(s4, fsa.arrow).register(all);
    s4.joint(s6, fsa.arrow).register(all);
    s6.joint(s2, fsa.arrow).register(all);
    fsa.arrow.label = null;	// empty label

}

function dimension(w, h)
{
    var world = document.getElementById('world');
    world.style.width = w + 'px';
    world.style.height = h + 'px';
}
