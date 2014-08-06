<?php

  if(count($_GET) > 0) {
    if(!empty($_GET['t'])) $t = basename($_GET['t']);
  }

  if(!isset($t)) $t = "Environment";
  if($t == '') $t = "Environment";

  $array = array(
    "2004" => "2004",
    "2005" => "2005&mdash;2007",
    "2007" => "2007&mdash;2011",
    "2011" => "2011&mdash;2014",
    "Economy" => "Economy &amp; Business",
    "Education" => "Education",
    "Environment" => "Environment &amp; Energy",
    "Health" => "Health",
    "Immigration" => "Immigration &amp; Integration",
    "Justice" => "Justice",
    "Welfare" => "Welfare");
  $class = array(
    "2004" => "",
    "2005" => "",
    "2007" => "",
    "2011" => "",
    "Economy" => "",
    "Education" => "",
    "Environment" => "",
    "Health" => "",
    "Immigration" => "",
    "Justice" => "",
    "Welfare" => "");
  $class[ $t ] = "here";
    
  // initial caption
  $caption = '<p>This graph shows Danish Members of Parliament (MPs) during ';
    
  if($t == "2004")
    $caption = $caption . 'year 2004';
  else if(strlen($t) == 4)
    $caption = $caption . 'years ' . $array[ $t ];
  else
    $caption = $caption . 'years 2004–2014';
  $caption = $caption .
      '. A link between two MPs indicates that they have cosponsored at least one bill on the selected theme.</p>' .
      '<div id="details"><h3><i class="fa fa-cube"></i> Details</h3>' .
      '<p>The graph contains /nodes MPs connected by /edges undirected edges' .
      ' and sized proportionally to their <a href="http://toreopsahl.com/tnet/weighted-networks/node-centrality/">weighted degree</a> in the network.</p>' .
      '<p>Each graph is based on a couple of hundred bills, mostly from opposition MPs, plus a handful of bills cosponsored by MPs from the government majority.</p>' .
      '<p>Group colors&nbsp;&nbsp; /colortext</p></div>';
?>
<!doctype html>
<html>
<head>
  <title>Cosponsorship networks in the Danish Parliament: Folketinget, <?php echo $t; ?></title>
  <meta charset="utf-8">
  <link href="http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,600" rel="stylesheet" type="text/css" />
  <link href="/assets/styles.css" rel="stylesheet" type="text/css" />
  <link rel="stylesheet" href="/assets/font-awesome-4.1.0/css/font-awesome.min.css">
  <style type="text/css" media="screen">
  html, body {
    font: 24px/150% "Source Sans Pro", sans-serif;
    background-color: #111;
    color: #fff;
    margin: 0;
    padding:0;
    width: 100%;
    height: 100%;
  }
  </style>
  <script type="text/javascript" src="/assets/jquery-1.11.1.min.js"></script>
  <script type="text/javascript" src="/assets/jquery.smart_autocomplete.min.js"></script>
  <script type="text/javascript" src="/assets/sigmajs-release-v1.0.2/sigma.min.js"></script>
  <script type="text/javascript" src="/assets/sigmajs-release-v1.0.2/plugins/sigma.parsers.gexf.min.js"></script>
  <script type="text/javascript" src="/assets/sigmajs-release-v1.0.2/plugins/sigma.layout.forceAtlas2.min.js"></script>
</head>
<body>

<div id="sigma-container">
  <div id="controls" class="bg_gr">
    
    <h1>danish parliament</h1>    

    <h2><a href="http://www.ft.dk/" title="Folketinget">
           <img src="logo_fo.png" height="18" alt="logo">
        </a>&nbsp;<?php echo $array[ $t ]; ?></h2>

    <p>Theme&nbsp;&nbsp;
      <!-- <a href="?time=2004" class='<?php echo $class["2004"]; ?>'>2004</a>&nbsp;&nbsp; -->
      <a href="?t=Economy" class='<?php echo $class["Economy"]; ?>'><?php echo $array["Economy"]; ?></a>&nbsp;&nbsp;
      <a href="?t=Education" class='<?php echo $class["Education"]; ?>'><?php echo $array["Education"]; ?></a>&nbsp;&nbsp;
      <a href="?t=Environment" class='<?php echo $class["Environment"]; ?>'><?php echo $array["Environment"]; ?></a>&nbsp;&nbsp;
      <a href="?t=Health" class='<?php echo $class["Health"]; ?>'><?php echo $array["Health"]; ?></a>&nbsp;&nbsp;
      <a href="?t=Immigration" class='<?php echo $class["Immigration"]; ?>'><?php echo $array["Immigration"]; ?></a>&nbsp;&nbsp;
      <a href="?t=Justice" class='<?php echo $class["Justice"]; ?>'><?php echo $array["Justice"]; ?></a>&nbsp;&nbsp;
      <a href="?t=Welfare" class='<?php echo $class["Welfare"]; ?>'><?php echo $array["Welfare"]; ?></a>
    </p>

    <p>Years&nbsp;&nbsp;
      <!-- <a href="?t=2004" class='<?php echo $class["2004"]; ?>'>2004</a>&nbsp;&nbsp; -->
      <a href="?t=2005" class='<?php echo $class["2005"]; ?>'>2005&mdash;07</a>&nbsp;&nbsp;
      <a href="?t=2007" class='<?php echo $class["2007"]; ?>'>2007&mdash;11</a>&nbsp;&nbsp;
      <a href="?t=2011" class='<?php echo $class["2011"]; ?>'>2011&mdash;14</a>
    </p>
    
    <!-- user search field -->
    <form action="/" method="post" class="search-nodes-form">
      <fieldset id="search-nodes-fieldset">
        <div></div>
      </fieldset>
    </form>

    <!-- buttons and sources -->
    <footer>

      <ul>
        <li>Click a node to show its ego network.</li>
        <li>Double click the graph to zoom in.</li>

        <!-- show/hide -->
        <li>
          Hide&nbsp;
          <label title="Do not draw network ties (vertex edges).">
            <input type="checkbox" id="showEdges" />
            Edges
          </label>
          &nbsp;
          <label title="Do not add labels to nodes (MP names) when zooming in.">
            <input type="checkbox" id="showLabels" />
            Labels
          </label>
          &nbsp;
          <label title="Draw only ties formed among frequent cosponsors (above mean edge weight).">
            <input type="checkbox" id="showSparse" />
            Weak ties
          </label>
        </li>
      </ul>

      <p><a href="#" id="recenter-camera" class="button" title="Reset graph to initial zoom position.">reset zoom</a>&nbsp;&nbsp;<a href="#" id="toggle-layout" class="button" title="Animate with Force Atlas 2.">Animate</a> <small><a href="https://gephi.org/2011/forceatlas2-the-new-version-of-our-home-brew-layout/" title="Details on the Force Atlas 2 algorithm."><i class="fa fa-info-circle"></i></a></small></p>

      <p><a href="http://twitter.com/share?text=Cosponsorship%20networks%20in%20the%20@Folketinget%20-%20Danish%20Parliament%20-%20using%20%23rstats%20and%20@sigmajs,%20by%20@phnk:&amp;url=<?php echo 'http://' . $_SERVER["SERVER_NAME"].$_SERVER["REQUEST_URI"]; ?>" class="button" title="Share this page on Twitter."><i class="fa fa-twitter"></i> Tweet</a>&nbsp;&nbsp;<a href="https://github.com/briatte/folketinget" class="button" title="Get the code and data from GitHub."><i class="fa fa-github"></i> Code</a></p>

      <ul>
        <li>Data from <a href="http://www.ft.dk/">Folketinget.dk</a> (summer 2014)</li>
      
        <li>Download&nbsp;&nbsp;<i class="fa fa-file-o"></i>&nbsp;&nbsp;<a href="<?php echo 'net_' . $t; ?>.gexf" title="Download this graph (GEXF, readable with Gephi)">network</a>&nbsp;&nbsp;<i class="fa fa-files-o"></i>&nbsp;&nbsp;<a href="folketinget.zip" title="Download all <?php echo $chamber; ?> graphs (GEXF, readable with Gephi)">full series</a>&nbsp;&nbsp;<i class="fa fa-file-image-o"></i>&nbsp;&nbsp;<a href="plots.html">plots</a></li>
      </ul>

      <div id="menu">
        <p><i class="fa fa-eye"></i>&nbsp;About</p>
        <ul>
          <li><h3>About</h3></li>
          <li class="intro">This visualization was built with <a href="http://www.r-project.org/" title="The R Project for Statistical Computing">R</a> and <a href="http://sigmajs.org/" title"JavaScript library dedicated to graph drawing">sigma.js</a>.</li>
          <li>It is part of a series inspired by <a href="http://coulmont.com/blog/2011/09/02/travail-de-deputes/">Baptiste Coulmont</a> and <a href="http://jhfowler.ucsd.edu/cosponsorship.htm">James Fowler</a>.</li>
          <li class="intro">Follow the links below to take a look at other parliaments.</li>
          <li><a href="/belparl">Belgium</a></li>
          <!-- <li><a href="/folketinget">Denmark</a></li> -->
          <li><a href="/epam">European Union</a></li>
          <li><a href="/neta">France</a></li>
          <li><a href="/stortinget">Norway</a></li>
          <li><a href="/riksdag">Sweden</a></li>
          <li><a href="/marsad">Tunisia</a></li>
        </ul>
      </div>

    </footer>

    <div id="graph-container"></div>
  </div>
  <div id="caption" class="bg_gr">
    <?php echo $caption; ?>
  </div>

</div>

<script>
function decimalAdjust(type, value, exp) {
	// If the exp is undefined or zero...
	if (typeof exp === 'undefined' || +exp === 0) {
		return Math[type](value);
	}
	value = +value;
	exp = +exp;
	// If the value is not a number or the exp is not an integer...
	if (isNaN(value) || !(typeof exp === 'number' && exp % 1 === 0)) {
		return NaN;
	}
	// Shift
	value = value.toString().split('e');
	value = Math[type](+(value[0] + 'e' + (value[1] ? (+value[1] - exp) : -exp)));
	// Shift back
	value = value.toString().split('e');
	return +(value[0] + 'e' + (value[1] ? (+value[1] + exp) : exp));
}

// Decimal round
if (!Math.round10) {
	Math.round10 = function(value, exp) {
		return decimalAdjust('round', value, exp);
	};
}

// Add a method to the graph model that returns an
// object with every neighbors of a node inside:
sigma.classes.graph.addMethod('neighbors', function(nodeId) {
  var k,
      neighbors = {},
      index = this.allNeighborsIndex[nodeId] || {};

  for (k in index)
    neighbors[k] = this.nodesIndex[k];

  return neighbors;
});

sigma.classes.graph.addMethod('getNeighborsCount', function(nodeId) {
  return this.allNeighborsCount[nodeId];
});

sigma.parsers.gexf(
  document.title.replace("Folketinget", "net_").replace(", ", "").replace("Cosponsorship networks in the Danish Parliament: ", "")+'.gexf',
  { // Here is the ID of the DOM element that
    // will contain the graph:
    container: 'sigma-container'
  },
  function(s) {

    // initial edges
    s.graph.edges().forEach(function(e) {
      e.originalColor = e.color;
      e.type = 'arrow';
    });
    
    // caption
    var parties = ["Enhedslisten", "Socialistisk Folkeparti", "Socialdemokratiet", "Radikale Venstre", "Kristendemokraterne", "Liberal Alliance", "Det Konservative Folkeparti", "Venstre", "Dansk Folkeparti", "Inuit Ataqatigiit", "Siumut", "Sambandsflokkurin", "Javnaðarflokkurin", "Independent"];
    var colors = new Array(parties.length);

    // initial nodes
    s.graph.nodes().forEach(function(n) {
      if(parties.indexOf(n.attributes["party"]) != -1)
        colors[ jQuery.inArray(n.attributes["party"], parties) ] = n.color;
      n.originalColor = n.color;
      n.originalX = n.x;
      n.originalY = n.y;
    });
    
    // caption text
    var t = "";
    for (i = 0; i < parties.length; i++) {
      if(typeof colors[i] != "undefined")
        t = t + "&nbsp;<span style='color:" +
          colors[i].replace('0.3)', '1)').replace('0.5)', '1)') + 
          "'>" + parties[i].replace(new RegExp(' ', 'g'), '&nbsp;') + "</span> ";
    };
        
    // pass network dimensions and caption
    document.getElementById('caption').innerHTML = document.getElementById('caption').innerHTML.replace('/nodes', s.graph.nodes().length).replace('/edges', s.graph.edges().length).replace('/colortext', t);
    
    // When a node is clicked, we check for each node
    // if it is a neighbor of the clicked one. If not,
    // we set its color as grey, and else, it takes its
    // original color.
    // We do the same for the edges, and we only keep
    // edges that have both extremities colored.
    s.bind('clickNode', function(e) {
      var nodeId = e.data.node.id,
          toKeep = s.graph.neighbors(nodeId);
      toKeep[nodeId] = e.data.node;
      
      s.graph.nodes().forEach(function(n) {
        if (toKeep[n.id])
          n.color = n.originalColor;
        else
          n.color = '#555';
      });

      s.graph.edges().forEach(function(e) {
        if (toKeep[e.source] && toKeep[e.target])
          e.color = e.originalColor;
        else
          e.color = '#333';
      });

      var profile = "<a href='http://www.ft.dk/Folketinget/findMedlem/" + e.data.node.attributes['url'] + ".aspx' title='Go to profile (Folkeinget, new window)' target='_blank'>";

      // distance
      var distance = "around " + e.data.node.attributes['distance'];
      if(isNaN(e.data.node.attributes['distance']))
        var distance = "impossible to compute (too isolated)";

      // transparency
      var rgba = e.data.node.color.replace('0.3)', '0.25)').replace('0.5)', '0.25)');
      
      document.getElementById('caption').innerHTML = '<p style="background:' + rgba + ';">' + 
        profile + '<img height="120px" src="http://www.ft.dk/Folketinget/findMedlem/' + 
        e.data.node.attributes['photo'] + '" alt="no photo available" /></a> You selected ' + profile + 
        e.data.node.label + '</a> <span title="Political party affiliation(s): ' + 
        e.data.node.attributes['party'] + '" style="color:' + rgba.replace('0.25)', '1)') + ';">(' + 
        e.data.node.attributes['party'] + ')</span>, an MP who had <span title="unweighted Freeman degree">' + s.graph.getNeighborsCount(nodeId) + 
        ' bill cosponsor(s)</span> on this theme.' +
        ' The <a href="http://toreopsahl.com/tnet/weighted-networks/shortest-paths/">mean weighted distance</a>' +
        ' between this MP and all others was&nbsp;' + distance + '.</p>';
      
      // Since the data has been modified, we need to
      // call the refresh method to make the colors
      // update effective.
      s.refresh();
    });

    // When the stage is clicked, we just color each
    // node and edge with its original color.
    s.bind('clickStage', function(e) {
      
      // caption
      var parties = ["Enhedslisten", "Socialistisk Folkeparti", "Socialdemokratiet", "Radikale Venstre", "Kristendemokraterne", "Liberal Alliance", "Det Konservative Folkeparti", "Venstre", "Dansk Folkeparti", "Inuit Ataqatigiit", "Siumut", "Sambandsflokkurin", "Javnaðarflokkurin", "Independent"];
      var colors = new Array(parties.length);
      
      s.graph.nodes().forEach(function(n) {
        n.color = n.originalColor;
        if(parties.indexOf(n.attributes["party"]) != -1)
          colors[ jQuery.inArray(n.attributes["party"], parties) ] = n.color;
      });

      s.graph.edges().forEach(function(e) {
        e.color = e.originalColor;
      });

      // Same as in the previous event:
      s.refresh();
      
      document.getElementById('caption').innerHTML = '<?php echo $caption; ?>';

      // pass network dimensions and caption (again)
      document.getElementById('caption').innerHTML = document.getElementById('caption').innerHTML.replace('/nodes', s.graph.nodes().length).replace('/edges', s.graph.edges().length).replace('/colortext', t);

    });
    
    s.settings({
      defaultEdgeColor: '#555',
      edgeColor: 'source',
      minNodeSize: 2,
      maxNodeSize: 6,
      defaultLabelColor: '#fff',
      defaultLabelSize: 18,
      font: "source sans pro",
      minEdgeSize: .3,
      maxEdgeSize: .9,
      labelHoverBGColor: 'node',
      defaultLabelHoverColor: '#fff',
      labelHoverShadow: 'node'
    });
    
    // autocomplete search field
    //
    $('#search-nodes-fieldset > div').remove();
    $('<div>' +
        '<label for="search-nodes">' +
          'Search' +
        '</label>' +
        '<input type="text" autocomplete="off" id="search-nodes"/>' +
      '</div>').appendTo('#search-nodes-fieldset');

    $('#search-nodes-fieldset #search-nodes').smartAutoComplete({
      source: s.graph.nodes().map(function(n){
        return n.label;
      })
    }).bind('itemSelect', function(e) {
      var label = e.smartAutocompleteData.item.innerText;

      // find node and neighbours
      var id = 0,
          nodeId = 0,
          toKeep = new Array();
      s.graph.nodes().forEach(function(n) {
        if (n.label == label) {
          id = n.id;
          nodeId = n.id,
          toKeep = s.graph.neighbors(nodeId);
        }
      });
            
      // color selected nodes
      s.graph.nodes().forEach(function(n) {
        if (n.id == id)
          n.color = n.originalColor;
        else if(toKeep[n.id])
          n.color = '#999';
        else
          n.color = '#555';
      });

      // color selected edges
      s.graph.edges().forEach(function(e) {
        if (toKeep[e.source] && toKeep[e.target])
          e.color = e.originalColor;
        else
          e.color = '#333';
      });

      s.refresh();
      
    });

    // protect search field
    //
    $('form.search-nodes-form').submit(function(e) {
      e.preventDefault();
    });
       
    // show it all, finally
    s.refresh();
    
    // hide edges
    //
    document.getElementById('showEdges').addEventListener('change',
    function(e){
      if (e.target.checked) {
        s.settings({
          drawEdges: false
        });
      } else {
        s.settings({
          drawEdges: true
        });
      }
      s.refresh();
    });
    
    // hide labels
    //
    document.getElementById('showLabels').addEventListener('change', 
    function(e){
      if (e.target.checked) {
        s.settings({
          drawLabels: false
        });
      } else {
        s.settings({
          drawLabels: true
        });
      }
      s.refresh();
    }); 
    
    // hide sparse ties
    //
    document.getElementById('showSparse').addEventListener('change', 
    function(e){
      var sum = 0;
      s.graph.edges().forEach(function(e) {
        sum = sum + e.weight;
      });
      sum = sum / s.graph.edges().length;
      if (e.target.checked) {
        s.graph.edges().forEach(function(e) {
          // use upper quartile marker
          if(e.weight < sum)
            e.color = 'rgba(66,66,66,0)';
        });
        s.settings({
          minEdgeSize: 0,
          maxEdgeSize: 2.7
        });
      } else {
        s.graph.edges().forEach(function(e) {
          e.color = e.originalColor;
        });
        s.settings({
          minEdgeSize: .3,
          maxEdgeSize: .9,
        });
      }
      s.refresh();
    }); 
        
    // view as map
    //
    // document.getElementById('showMap').addEventListener('change',
    // function(e){
    //   if (e.target.checked) {
    //     s.graph.nodes().forEach(function(n) {
    //
    //       // map node position to lon/lat
    //       n.x = n.attributes['lon'];
    //       n.y = n.attributes['lat'] * -1;
    //
    //     });
    //   } else {
    //     s.graph.nodes().forEach(function(n) {
    //       n.x = n.originalX;
    //       n.y = n.originalY;
    //     });
    //   }
    //   s.refresh();
    // });
    
    // force atlas
    //
    document.getElementById('toggle-layout').addEventListener('click', 
    function() {
      if ((s.forceatlas2 || {}).isRunning) {
        s.stopForceAtlas2();
        document.getElementById('toggle-layout').innerHTML = 'Animate';
      } else {
        s.startForceAtlas2();
        document.getElementById('toggle-layout').innerHTML = 'Stop';
      }
    });
    
    // reset zoom
    //
    document.getElementById('recenter-camera').addEventListener('click',
    function() {
      s.cameras[0].goTo({
                x: 0,
                y: 0,
                angle: 0,
                ratio: 1
              });
    });
    
  }
);
</script>

</body>
</html>
