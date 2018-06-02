var edges;
var nodes;
var links;
var shownNodes;
var shownEdges;
var partitions;
var mydata;
var topos;
const normalSwitchColor = {background: "grey"};
const normalHostColor = {background: "red"};
const normalEdgeColor = {color: "grey", highlight: "blue"};
const paletteNone = ["red"];
const palette9 = ["blue", "orange", "green", "pink", "brown", "purple", "yellow", "red", "gray"];
const palette11 = ["#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f",
                   "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99"];
var nodesWithPhysics;
var edgesWithPhysics;
var nodeBandwidthList;
var edgeBandwidthList;
var network;
var description = "";
const lastColorMode = "bandwidth";

function filterEdge(e) {
    return (shownNodes.get(e.to) && shownNodes.get(e.from));
}

function updateDescription() {
    /* update the desciption of the graph */
    var num_hosts = shownNodes.get({filter: function(n) {
        return n.type == "host";
    }}).length;
    var num_switches = shownNodes.length-num_hosts;
    var enabledTopos = shownNodes.distinct("topo");
    var shownTopos = enabledTopos.reduce(function(a, e) {
        if (e != -1)
            a.push(topos[e]);
        return a;
    }, []);

    description =
        '<h2>Stats</h2>' +
        shownNodes.length + ' nodes\n' +
        shownEdges.length + ' edges\n' +
        num_hosts + ' hosts\n' +
        num_switches + ' switches\n' +
        '<h2>Partitions</h2>' +
        '<ul><li>' + partitions.join('</li><li>') + '</li></ul>' +
        '<h2>Hwloc topologies</h2>' +
        '<ul><li>' + shownTopos.sort().join('</li><li>') + '</li></ul>' +
        '<div id="desc_selected"></div>';
    document.getElementById('description').innerHTML = description;
}

function printWithTime(msg) {
    if (typeof printWithTime.lastTime == 'undefined' ) {
        printWithTime.lastTime = Math.floor(new Date().getTime()/1000);
    }

    var time = Math.floor(new Date().getTime()/1000);
    var duration = time-printWithTime.lastTime;
    printWithTime.lastTime = time;
    console.log(msg);
    console.log(duration + "s elapsed");
}

function handleFileSelect(evt) {
    var files = evt.target.files;
    var fr = new FileReader();
    fr.onload = function(e) {
        printWithTime("Starting...");
        data = e.target.result;
        loadFile(data);
    };
    fr.readAsText(files[0]);
}

function renameKey(obj, newkey, oldkey)
{
    return obj.map(function(d) { d[newkey] = d[oldkey]; delete d[oldkey]; return d; });
}

function getNeighbours(node)
{
    return node.edges.map(function(e) {
        return edges.get(e).to;
    }).filter(function(n) {
        return shownNodes.get(n);
    });
}

function palette(size) {
    var palette = [];

    if (!size)
        return paletteNone;

    if (size <= 9)
        return palette9.slice(0, size+1);

    if (size <= 11)
        return palette11.slice(0, size+1);

    var v = Math.ceil(Math.pow(size+1, 1/3));
    var value = 255/v;
    for( var rStep = 0, r = 0; rStep < v; rStep++) {
        for( var gStep = 0, g = 0; gStep < v; gStep++ ) {
            for( var bStep = 0, b = 0; bStep < v; bStep++ ) {
                if (!r && !g && !b)
                    continue;
                palette.push('rgb(' + Math.round(r) + ', ' + Math.round(g) + ', '
                             +  Math.round(b) + ')');
                b += value;
            }
            g += value;
        }
        r += value;
    }
    return palette;
}

function loadFile(data)
{
    printWithTime("File loaded");
    mydata = JSON.parse(data);
    printWithTime("JSON parsed");
    partitions = mydata.partitions;
    topos = mydata.hwloctopos;

    /* Prepare UI */
    /***********************************************************************/
    /* Add elements to selectColors */
    var selectColors = document.getElementById("selectColors");
    while (selectColors.length > 1) {
        selectColors.removeChild(selectColors.lastChild);
    }
    {
        var el = document.createElement("option");
        el.textContent = "normal";
        el.value = "normal";
        selectColors.appendChild(el);
    }
    if (partitions.length) {
        var el = document.createElement("option");
        el.textContent = "partition";
        el.value = "partition";
        selectColors.appendChild(el);
    }
    if (topos.length) {
        var el = document.createElement("option");
        el.textContent = "hwloc";
        el.value = "hwloc";
        selectColors.appendChild(el);
    }
    {
        var el = document.createElement("option");
        el.textContent = "bandwidth";
        el.value = "bandwidth";
        selectColors.appendChild(el);
    }
    {
        var el = document.createElement("option");
        el.textContent = "bandwidth_check";
        el.value = "bandwidth_check";
        selectColors.appendChild(el);
    }

    /* Add elements to selectPartition */
    var selectPartition = document.getElementById("selectPartition");
    while (selectPartition.length > 1) {
        selectPartition.removeChild(selectPartition.lastChild);
    }
    var partitionOptions = partitions.slice();
    partitionOptions.unshift("All");
    partitionOptions.forEach(function(partition, i) {
        var el = document.createElement("option");
        el.textContent = partition;
        el.value = i-1; // because of "All"
        selectPartition.appendChild(el);
    });
    /***********************************************************************/

    /* Set  edges */
    /***********************************************************************/
    mydata.edges.map(
        function(e) {e["label"] = Math.round(parseFloat(e["gbits"]));});
    var gbits_min = Math.min.apply(null, mydata.edges.map(function(e) { return e["gbits"];}));
    mydata.edges.map(
        function(e) {e["width"] = e["gbits"]/gbits_min; e["physics"] = false;});
    edges = new vis.DataSet({});
    edges.add(mydata.edges);
    printWithTime("Dataset edges created");
    /***********************************************************************/

    /* Set  nodes */
    /***********************************************************************/
    mydata.nodes = renameKey(mydata.nodes, "title", "desc");
    mydata.nodes.forEach( function(n) {
        n["color"] = n["type"] == "host" ? normalHostColor : normalSwitchColor;
        n["bandwidth"] =
            edges.get(n["edges"]).map(function(e) { if (!e) return 8; else return e.gbits; })
            .reduce(function(a, b) { return a + b; }, 0);
        n["size"] = 10*Math.log(n["bandwidth"]);
        n["x"] = n["y"] = 0;
        if (mydata.type === "tree") {
            n["fixed"] = true;
            n["physics"] = true;
        }
    });
    nodes = new vis.DataSet({});
    nodes.add(mydata.nodes);
    printWithTime("Dataset nodes created");
    /***********************************************************************/

    /* Set  links */
    /***********************************************************************/
    links = new vis.DataSet({});
    links.add(mydata.links);
    printWithTime("Dataset links created");
    /***********************************************************************/

    /* Build list of width for nodes */
    nodeBandwidthList = [];
    nodes.forEach(function(n) {
        if (nodeBandwidthList.indexOf(n.size) == -1) {
            nodeBandwidthList.push(n.size);
        }
    });
    /* Build list of width for edges */
    edgeBandwidthList = [];
    edges.forEach(function(e) {
        if (edgeBandwidthList.indexOf(e.gbits) == -1) {
            edgeBandwidthList.push(e.gbits);
        }
    });
}

function nodeColors()
{
    var e = document.getElementById("selectColors");
    var colorMode = e.options[e.selectedIndex].value;

    if (colorMode == "normal") {
        shownNodes.forEach(function(n) {
            if (n.type == "host")
                shownNodes.update({id: n.id, color: normalHostColor});
            else
                shownNodes.update({id: n.id, color: normalSwitchColor});
        });

        if (lastColorMode == "bandwidth" || lastColorMode == "bandwidth_check") {
            shownEdges.forEach(function(n) {
                shownEdges.update({id: n.id, color: normalEdgeColor});
            });
        }

    } else if (colorMode == "partition") {
        var nodeColors = palette(partitions.length);
        shownNodes.forEach(function(n) {
            if (n.type == "host") {
                var colorIdx = n.part[0];
                shownNodes.update({id: n.id, color: {background: nodeColors[colorIdx]}});

            } else
                shownNodes.update({id: n.id, color: normalSwitchColor});
        });

        if (lastColorMode == "bandwidth" || lastColorMode == "bandwidth_check") {
            shownEdges.forEach(function(n) {
                shownEdges.update({id: n.id, color: normalEdgeColor});
            });
        }

    } else if (colorMode == "hwloc") {
        var nodeColors = palette(topos.length);
        shownNodes.forEach(function(n) {
            if (n.type == "host") {
                var colorIdx = n.topo;
                if (colorIdx != -1) {
                    shownNodes.update({id: n.id, color: {background: nodeColors[colorIdx]}});
                }

            } else
                shownNodes.update({id: n.id, color: normalSwitchColor});
        });

        if (lastColorMode == "bandwidth" || lastColorMode == "bandwidth_check") {
            shownEdges.forEach(function(n) {
                shownEdges.update({id: n.id, color: normalEdgeColor});
            });
        }

    } else if (colorMode == "bandwidth") {
        var nodeColors = palette(nodeBandwidthList.length);
        shownNodes.forEach(function(n) {
            var colorIdx = nodeBandwidthList.indexOf(n.size);
            if (colorIdx != -1) {
                shownNodes.update({id: n.id, color: {background: nodeColors[colorIdx]}});
            }
        });
        var edgeColors = palette(edgeBandwidthList.length);
        shownEdges.forEach(function(n) {
            var colorIdx = edgeBandwidthList.indexOf(n.gbits);
            if (colorIdx != -1) {
                shownEdges.update({id: n.id, color: {color: edgeColors[colorIdx]}});
            }
        });

    } else if (colorMode == "bandwidth_check") {
        var edgeColors = palette(edgeBandwidthList.length);
        shownEdges.forEach(function(n) {
            var colorIdx = edgeBandwidthList.indexOf(n.gbits);
            if (colorIdx != -1) {
                shownEdges.update({id: n.id, color: {color: edgeColors[colorIdx]}});
            }
        });
        shownNodes.forEach(function(n) {
            /* Quick check about in bw = out bd */
            if (n.edges.length == 1) {
                var colorIdx = edgeBandwidthList.indexOf(n.bandwidth);
                shownNodes.update({id: n.id, color: {background: edgeColors[colorIdx]}});

            } else {
                var colorIdx = edgeBandwidthList.indexOf(n.bandwidth/2);
                if (colorIdx != -1) {
                    shownNodes.update({id: n.id, color: {background: edgeColors[colorIdx]}});
                } else {
                    shownNodes.update({id: n.id, color: {background: "black"}});
                }
            }
        });
    }
    lastcolorMode = colorMode;
}

function selectNodesAndEdges(field, value)
{
    var regexp = new RegExp(value, "g");
    var nodeIds;
    var nodeSet;
    var edgeIds;
    var edgeSet;

    switch (field) {
    case "part":
        var strings = partitions;
        matchingStrings = strings.reduce(function(a, e, i) {
            if (e.match(regexp))
                a.push(i);
            return a;
        }, []);
        nodeSet = shownNodes.get({filter: function(n) {
            for (var e = 0; e < n[field].length; e++) {
                var elem = n[field][e];
                for (var f = 0; f < matchingStrings.length; f++) {
                    if (elem == matchingStrings[f])
                        return true;
                }
            }
            return false;
        }});
        edgeSet = shownEdges.get({filter: function(n) {
            for (var e = 0; e < n[field].length; e++) {
                var elem = n[field][e];
                for (var f = 0; f < matchingStrings.length; f++) {
                    if (elem == matchingStrings[f])
                        return true;
                }
            }
            return false;
        }});
        break;
    case "topo":
        var strings = topos;
        matchingStrings = strings.reduce(function(a, e, i) {
            if (e.match(regexp))
                a.push(i);
            return a;
        }, []);
        nodeSet = shownNodes.get({filter: function(n) {
            var elem = n[field];
            for (var f = 0; f < matchingStrings.length; f++) {
                if (elem == matchingStrings[f])
                    return true;
            }
            return false;
        }});
        edgeSet = [];
        break;
    default:
        nodeSet = shownNodes.get({filter: function(n) {
            return n[field] ? n[field].toString().match(regexp): false;
        }});
        edgeSet = shownEdges.get({filter: function(e) {
            return e[field] ? e[field].toString().match(regexp): false;
        }});
    }
    nodeIds = nodeSet.map(function(n) { return n.id; });
    edgeIds = edgeSet.map(function(e) { return e.id; });

    network.setSelection({edges: edgeIds, nodes: nodeIds}, {highlightEdges: false});
    showSelectedNodes();
}

function showSelectedEdges()
{
    var infos = "";

    var edgeIds = network.getSelectedEdges();
    var nodeIds = network.getSelectedNodes();

    infos += '<h1>' + edgeIds.length + ' edges selected</h1>';
    if (!nodeIds.length) {
        var neighbours = [];
        edgeIds.forEach(function(edgeId, id) {
            var edge = edges.get(edgeId);
            var dest = nodes.get(edge.to);
            var src = nodes.get(edge.from);

            infos += '<h2>edge #' + (id+1) + '</h2>' +
                'ID: ' + edge.id + '\n' +
                'Partitions: ' + edge.part.map(function(p){return partitions[p]})
                .join(', ') + '\n' +
                '\t' + src.title + '(' + src.id + ') - ' +
                dest.title + '(' + dest.id + '): ' +
                edge.label + 'Gb\n';

            var edgeLinks = edge.links;
            edgeLinks.forEach(function(linkId) {
                var link = links.get(linkId);
                infos += '\t\tlink ' + linkId + ' ('
                    + link.src_port + '->' + link.dst_port + '):'
                    + link.gbits + 'Gb\n';
            });
        });
    }
    document.getElementById('desc_selected').innerHTML += infos;
}

function showSelectedNodes()
{
    document.getElementById('desc_selected').innerHTML = '';
    var infos = "";

    var edgeIds = network.getSelectedEdges();
    var nodeIds = network.getSelectedNodes();

    var neighbourLists = [];
    var similarNeighbours = null;
    if (nodeIds.length) {
        infos += '<h1>' + nodeIds.length + ' nodes selected</h1>';

        var inputNodes = shownNodes.get(nodeIds);
        inputNodes.forEach(function(node) {
            var nodeEdges = node.edges;
            var neighbourText = [];
            var neighbours = [];

            nodeEdges.forEach(function(edgeId) {
                var edge = edges.get(edgeId);
                var dest = nodes.get(edge.to);

                var s = '\t' + dest.title + ' (' + dest.id + ') - edgeId '
                    + edgeId + ': ' +  edge.label + 'Gb\n';

                var edgeLinks = edge.links;
                edgeLinks.forEach(function(linkId) {
                    var link = links.get(linkId);
                    s += '\t\tlink ' + linkId + ' ('
                        + link.src_port + '->' + link.dst_port + '):'
                        + link.gbits + 'Gb\n';
                });
                neighbourText.push(s);
                neighbours.push(dest.id);
            });

            sub = node.sub.map(function(s) {
                return nodes.get(s).title;
            });

            infos +=
                '<h2>' + node.title + '</h2>' +
                'ID: ' + node.id + ' Hostname: ' + node.hostname + '\n' +
                'Partitions: ' + node.part.map(function(p){return partitions[p]})
                .join(', ') + '\n' +
                'Subnodes: ' + sub.join(', ') + '\n' +
                neighbourText.length + ' neighbours:\n' + neighbourText.sort().join('') +
                'Total bandwidth: ' + node.bandwidth + '\n' +
                ((node.topo != -1) ? ('Hwloc topology: ' + topos[node.topo]): '');

            if (!similarNeighbours) {
                similarNeighbours = neighbours;
            } else {
                similarNeighbours = similarNeighbours.filter(function(n) {
                    return neighbours.indexOf(n) != -1;
                });
            }
            neighbourLists.push(neighbours);
        });
    }

    if (nodeIds.length > 1) {
        var s = [];
        similarNeighbours.forEach(function(n) {
            s.push('\t' + nodes.get(n).title + '\n');
        });
        infos +=
            '<h2>Similar neighbours</h2>' +
            similarNeighbours.length + ' similar neighbours:\n' + s.sort().join('');
        infos +=
            '<h2>Specific neighbours</h2>';
        nodeIds.forEach(function(n, idx) {
            infos += '\t' + shownNodes.get(n).title + ": " +
                neighbourLists[idx]
                .filter(function(n)
                        {return similarNeighbours.indexOf(n) < 0;})
                .map(function(n)
                     {return shownNodes.get(n).title;})
                .sort().join(', ') +
                '\n';
        });
    }

    document.getElementById('desc_selected').innerHTML += infos;
}

function search()
{
    var selectSearch = document.getElementById("selectSearch");
    var field = selectSearch.options[selectSearch.selectedIndex].value;

    if (field == "") {

    } else {
        var searchValue = document.getElementById("searchValue");
        var value = searchValue.value;
        selectNodesAndEdges(field, value);
    }
}

function draw()
{
    printWithTime("Draw");
    var selectPartition = document.getElementById("selectPartition");
    var partition = parseInt(
        selectPartition.options[selectPartition.selectedIndex].value);

    var subNodes = new vis.DataSet(nodes.get({filter: function (n) { return n.merged == 0 }}));
    printWithTime("Nodes filtered");

    var subEdges;
    if (partition == -1) { // for "All"
        shownNodes = subNodes;
        subEdges = new vis.DataSet(edges.get({filter: filterEdge}));
    } else {
        shownNodes = new vis.DataSet(subNodes.get({filter: function (n) {
            return n.part.indexOf(partition) > -1;
        }}));
        subEdges = new vis.DataSet(edges.get({filter: function (e) {
            return shownNodes.get(e.to) && shownNodes.get(e.from)
                    && e.part.indexOf(partition) > -1;
        }}));
    }
    printWithTime("subdatasets created");

    /* Keep edges in only one way */
    shownEdges = new vis.DataSet(subEdges.get({filter: function (e) {
        //return (subNodes.get(e.from) && subNodes.get(e.from).type != "host"); // DEBUG XXX
        /* We'd rather keep the edge where e.from > e.to, so we would keep the
           ones from virtual nodes which would ease the expansion.
        */
        if (e.from > e.to) {
            if (!subEdges.get(e.reverse)) {
                e.arrows = {to: true};
            }
            return true;

        } else if (!subEdges.get(e.reverse)) {
            e.arrows = {to: true};
            return true;

        } else
            return false;
    }}));
    printWithTime("Edges filtered (keep one way)");

    var physicsEnabled = true;
    var selectColor = document.getElementById("selectColors");
    var colorMode =
        selectColor.options[selectColor.selectedIndex].value;
    if (colorMode != "")
        nodeColors();
    else
        selectColor.selectedIndex = 1;

    updateDescription();
    printWithTime("Description updated");

    /* Set nodes positions */
    if (mydata.type === "tree"){
        physicsEnabled = false;
        var stack = [];
        var current_ring;
        var node_pool = new vis.DataSet(shownNodes.get().map( function (n) {
            n.crt_nedges = getNeighbours(n).length;
            n.subtreeSize = n.size * Math.PI * 2;
            n.initAngle = 0;
            return n;
        }));
        while (0 < node_pool.length) {
            current_ring = node_pool.get({filter: function (n) { return 1 >= n.crt_nedges }});
            if (current_ring.length > 0) {
                node_pool.remove(current_ring);
                current_ring.forEach( function (node) {
                    node_pool.get(
                        getNeighbours(node),
                        {returnType: 'Array', filter: function (nbg) { return null !== nbg }}
                    ).forEach( function (nbg) {
                        nbg.subtreeSize += node.subtreeSize / 2 / Math.PI * 2.3;
                        nbg.crt_nedges--;
                        node_pool.update(nbg);
                    });
                });
            } else {
                /* tree "root" is made of multiple nodes */
                /* Step 1: Extract any node not connected to the last
                 * saved stack level.
                 */
                var substack = [];
                current_ring = node_pool.get({filter: function (n) {
                    return 0 === stack[stack.length-1].get(
                        getNeighbours(n),
                        {returnType: 'Array',
                         filter: function (nbg) { return null !== nbg }
                        }).length}});
                node_pool.remove(current_ring);
                substack.push(new vis.DataSet(current_ring));
                current_ring.forEach( function (n) {
                    node_pool.get(getNeighbours(n),{
                        returnType:'Array',
                        filter: function (nbg) { return null !== nbg }
                    }).forEach( function (nbg) {
                        nbg.crt_nedges--;
                        node_pool.update(nbg);
                    });
                });
                while (current_ring.length > 0) {
                    current_ring = substack[substack.length-1].get({
                        filter: function (n) {
                            return 0 === substack[substack.length-1].get(
                                getNeighbours(n),
                                {returnType: 'Array',
                                 filter: function (nbg) { return null !== nbg }
                                }).length}});
                    if (current_ring.length > 0) {
                        substack[substack.length-1].remove(current_ring);
                        substack.push(new vis.DataSet(current_ring));
                    }
                }
                /* Step 2: Consider nodes with highest neighbors in
                 * remaining pool to be closer to the root.
                 */
                var max_nedges = node_pool.get()
                    .reduce(function (r, n) {
                        return Math.max(r, n.crt_nedges)
                    }, -Infinity);
                while(max_nedges > 0) {
                    current_ring = node_pool.get({
                        filter: function(n){ return max_nedges <= n.crt_nedges }
                    });
                    node_pool.remove(current_ring);
                    substack.push(new vis.DataSet(current_ring));
                    current_ring.forEach( function (n) {
                        node_pool.get(getNeighbours(n),{
                            returnType:'Array',
                            filter: function (nbg) { return null !== nbg }
                        }).forEach( function (nbg) {
                            nbg.crt_nedges--;
                            node_pool.update(nbg);
                        });
                    });
                    max_nedges = node_pool.get()
                        .reduce( function (r,n) {
                            return Math.max(r,n.crt_nedges)
                        }, -Infinity);
                }

                if (node_pool.length > 0) { /* Add remaining nodes */
                    current_ring = node_pool.get({returnType: 'Array'});
                    node_pool.remove(current_ring);
                    substack.push(new vis.DataSet(current_ring));
                }
                /* Propagate subtree sizes */
                while(substack.length > 1) {
                    var sons = substack.pop();
                    sons.get({returnType: 'Array'}).forEach( function (n) {
                        substack[substack.length-1].get(getNeighbours(n), {
                            returnType:'Array',
                            filter: function (nbg) { return null !== nbg }
                        }).forEach( function (nbg) {
                            nbg.subtreeSize += n.subtreeSize / 2 / Math.PI * 2.3;
                            substack[substack.length-1].update(nbg);
                        });
                    });
                    stack.push(sons);
                }
                current_ring = substack.pop().get({returnType: 'Array'});
            }
            stack.push(new vis.DataSet(current_ring));
        }
        printWithTime("Tree built");
        /* If root is made of multiple nodes, separate them around 0 */
        if (stack[stack.length-1].length > 1) {
            /* Step 3: Display multiple root around 0, and
             * corresponding subtrees around as a single circle.
             */
            var ds = stack.pop();
            var subtreeSize = ds.get().reduce( function (r, n) {
                return r + n.size * 2
            }, 0) / 2 / Math.PI;
            var subsubtreeSize = Math.max(ds.get().reduce( function (r, n) {
                return r + n.subtreeSize / 2 / Math.PI
            }, 0) / 3 / Math.PI,stack[stack.length-1].get().reduce( function (r, n) {
                return r + n.size
            }, 0) / Math.PI, subtreeSize * 2 * Math.PI);
            if (stack.length == 1) {
                var arrBig = ds.get().map(function (n) {
                    return {id: n.id, nb_neighbours: stack[stack.length-1].get(getNeighbours(n), {
                        returnType: 'Array',
                        filter: function(nbg) { return null !== nbg && nbg.x === 0 && nbg.y === 0}
                    }).length}
                }).sort(function (a,b) {
                    return a.nb_neighbours < b.nb_neighbours ? -1
                        : a.nb_neighbours > b.nb_neighbours ? 1 : 0;
                });
                var arrSmall = arrBig.splice(ds.length/2,ds.length/2).reverse();
                var arrOrdered = [];
                for (var i=0, l=arrSmall.length; i<l; i++) {
                    arrOrdered.push(arrBig[i]);
                    arrOrdered.push(arrSmall[i]);
                }
                if (1 === ds.length % 2) arrOrdered.push(arrBig[arrBig.length-1]);
                ds.get(arrOrdered.map(function(o){return o.id})).forEach( function (root, id, arr) {
                    var angle = id * 2 * Math.PI / arr.length
                    shownNodes.update({
                        id: root.id,
                        x: Math.cos(angle) * subtreeSize,
                        y: Math.sin(angle) * subtreeSize
                    });
                    stack[stack.length-1].get(getNeighbours(root), {
                        returnType: 'Array',
                        filter: function(nbg) { return null !== nbg && nbg.x === 0 && nbg.y === 0}
                    }).forEach( function (child, childid, children) {
                        var subid = childid - children.length / 2;
                        var subangle = angle + subid * 2 * Math.PI / stack[stack.length-1].length;
                        /* Positionned only once */
                        stack[stack.length-1].update({
                            id: child.id,
                            initAngle: subangle,
                            x: Math.cos(subangle) * subsubtreeSize,
                            y: Math.sin(subangle) * subsubtreeSize
                        });
                    });
                });
            } else {
                ds.get().forEach( function (root, id, arr) {
                    var angle = id * 2 * Math.PI / arr.length
                    shownNodes.update({
                        id: root.id,
                        x: Math.cos(angle) * subtreeSize,
                        y: Math.sin(angle) * subtreeSize
                    });
                    stack[stack.length-1].get().forEach( function (child, childid, children) {
                        var angle = childid * 2 * Math.PI / children.length;
                        /* Positionned only once */
                        stack[stack.length-1].update({
                            id: child.id,
                            initAngle: angle,
                            x: Math.cos(angle) * subsubtreeSize,
                            y: Math.sin(angle) * subsubtreeSize
                        });
                    });
                });
            }
        }
        
        stack.reverse().forEach( function (ds, lvl, stack) {
            /* Careful: stack is reversed, children are in lvl+1 */
            
            ds.get().forEach( function (node) {
                if (node.id !== undefined) {
                    shownNodes.update({
                        id: node.id,
                        x: node.x,
                        y: node.y,
                    });
                }
                if (lvl < stack.length - 1) { /* Update if not penultimate level */
                    stack[lvl+1].get(getNeighbours(node), {
                        filter: function (n) { return n !== null && n.x === 0 && n.y === 0}
                    }).forEach( function (child, id, arr) {
                        var angle = node.initAngle + id * 2 * Math.PI / arr.length +
                            (arr.length % 2 ? 0 : Math.PI / arr.length);
                        var dist = node.subtreeSize / 2 / Math.PI;
                        /* Positionned only once */
                        stack[lvl+1].update({
                            id: child.id,
                            initAngle: angle,
                            x: node.x + Math.cos(angle) * dist,
                            y: node.y + Math.sin(angle) * dist
                        });
                    });
                }
            });
        });
        printWithTime("Positions pre-computed");
    }

    /* create a network */
    var container = document.getElementById('mynetwork');
    var data = {
        nodes: shownNodes,
        edges: shownEdges
    };

    var options = {interaction:{hover: true, tooltipDelay: 0, multiselect: true},
                   layout:{improvedLayout: false},
                   physics: {enabled: physicsEnabled, barnesHut: {gravitationalConstant: -15000}},
                   nodes: {shape: 'dot', borderWidth: 0.1, borderWidthSelected: 5},
                   edges: {color: normalEdgeColor}
                  };
    network = new vis.Network(container, data, options);

    network.on("selectNode", function (params) {
        params.nodes.forEach(function(nodeId) {
            shownNodes.update({id: nodeId, fixed: false});
        });
        showSelectedNodes();
        showSelectedEdges();
    });
    network.on("deselectNode", function (params) {
        document.getElementById('description').innerHTML = description;
    });

    network.on("selectEdge", function (params) {
        showSelectedNodes();
        showSelectedEdges();
    });
    network.on("deselectEdge", function (params) {
        document.getElementById('description').innerHTML = description;
    });

    if (mydata.type === "tree") {
        network.once("afterDrawing", function () {
            this.setOptions({physics: {enabled: true}});
        });
    }

    if (1) { // DEBUG XXX
        network.on("dragStart", function (params) {
            params.event = "[original event]";
            var nodeIds = params.nodes;
            var edgeIds = params.edges;

            nodesWithPhysics = [];
            nodeIds.forEach(function (id) {
                shownNodes.update({id: id, physics: true, fixed: false});
                nodesWithPhysics.push(id);
            });
            edgesWithPhysics = edgeIds;
            edgeIds.forEach(function(edgeId) {
                shownEdges.update({id: edgeId, physics: true});
                var edge = shownEdges.get(edgeId);
                var node = shownNodes.get(edge.to);
                if (1 >= node.edges.length) {
                    nodesWithPhysics.push(edge.to);
                    shownNodes.update({id: edge.to, physics: true, fixed: false});
                }
                var node = shownNodes.get(edge.from);
                if (1 >= node.edges.length) {
                    nodesWithPhysics.push(edge.from);
                    shownNodes.update({id: edge.from, physics: true, fixed: false});
                }
            });
        });
        network.on("dragEnd", function (params) {
            edgesWithPhysics.forEach(function(edgeWithPhysic) {
                shownEdges.update({id: edgeWithPhysic, physics: false, fixed: true});
            });
            nodesWithPhysics.forEach(function(nodeWithPhysic) {
                shownNodes.update({id: nodeWithPhysic, physics: false, fixed: true});
            });
        });
    }

    network.on("stabilizationIterationsDone", function () {
        //network.setOptions({
        //    nodes: {physics: false},
        //    edges: {physics: false},
        //    layout: {improvedLayout: false},
        //});
        shownEdges.getIds().forEach(function(e) {
            shownEdges.update({id: e, physics: false});
        });
        shownNodes.getIds().forEach(function(n) {
            shownNodes.update({id: n, physics: false});
        });
    });
}

function expandSelected()
{
    nodeIds = network.getSelectedNodes();
    nodeIds.forEach(function(n) {
        // TODO
        node = shownNodes.get(n);
        neighbours = getNeighbours(node);

        /* Physics enabled for neighbours */
        neighbours.forEach(function(nbg) {
            printWithTime(nbg + " -> physics");
            shownNodes.update({id: nbg, physics: true, fixed: true});
        });

        /* The selected nodes are removed (then replaced) */
        shownEdges.remove(node.edges);
        printWithTime(shownNodes.remove(node) + " -> removed");
        var selectedNodes = [];
        node.sub.forEach(function(s) {
            subnode = nodes.get(s);
            subnode.physics = false;
            shownNodes.add(subnode);
            shownEdges.add(edges.get(subnode.edges), {filter: filterEdge});
            selectedNodes.push(subnode.id);
        });
        network.selectNodes(selectedNodes);
        showSelectedNodes();
    });
    updateDescription();
}

function mergeNode(node)
{
    // TODO
}

function prepareImage()
{
    function export_SVG() {

        function Node (x, y, r, c) {
            var cx, cy, radius, fill;
            this.node = document.createElementNS('http://www.w3.org/2000/svg','circle');
            this.attribute = function (key,val) {
                if (val === undefined) return this.node.getAttribute(key);
                this.node.setAttribute(key,val);
                return val;
            }
            this.getCoord = function () {return {cx: cx, cy: cy}}
            this.setCoord = function (obj) {
                if (undefined === obj) return;
                cx = this.attribute('cx', obj.cx);
                cy = this.attribute('cy', obj.cy);
            }
            this.getSize = function () {return r}
            this.setSize = function (r) {radius = this.attribute('r', r)}
            this.getColor = function () {return fill}
            this.setColor = function (c) {fill = this.attribute('fill', c)}
            // Init
            cx     = this.attribute.apply(this, ['cx', x]);
            cy     = this.attribute.apply(this, ['cy', y]);
            radius = this.attribute.apply(this, ['r', r]);
            fill   = this.attribute.apply(this, ['fill', c]);
        }

        function Edge (x1, y1, x2, y2, w, c, l) {
            var from, to, width, label, color;
            this.edge = document.createElementNS('http://www.w3.org/2000/svg','line');
            this.attribute = function (key,val) {
                if (val === undefined) return this.edge.getAttribute(key);
                this.edge.setAttribute(key,val);
                return val;
            }
            this.style = function (key,val) {
                if (undefined === val) return this.edge.style[key];
                this.edge.style[key] = val;
                return val;
            }
            this.getCoords = function () {return {from: from, to: to}}
            this.setCoords = function (obj) {
                if (undefined === obj) return;
                from.x = this.attribute('x1', obj.from === undefined ? undefined : obj.from.x);
                from.y = this.attribute('y1', obj.from === undefined ? undefined : obj.from.y);
                to.x   = this.attribute('x2', obj.to   === undefined ? undefined : obj.to.x);
                to.y   = this.attribute('y2', obj.to   === undefined ? undefined : obj.to.y);
            }
            this.getWidth = function () {return width}
            this.setWidth = function (w) {width = this.style('stroke-width', w)}
            this.getColor = function () {return color}
            this.setColor = function (color) {fill = this.style('stroke', color)}
            // TO BE ADDED: label
            // Init
            from   = {x: this.attribute.apply(this, ['x1', x1]),
                      y: this.attribute.apply(this, ['y1', y1])};
            to     = {x: this.attribute.apply(this, ['x2', x2]),
                      y: this.attribute.apply(this, ['y2', y2])};
            width  = this.style.apply(this, ['stroke-width', w]);
            color  = this.style.apply(this, ['stroke', c]);
            label  = l
        }

        var nodeArr = shownNodes.get();
        var bounds = nodeArr.reduce( function (bounds, node) {
            return {minx: Math.min(bounds.minx, node.x), maxx: Math.max(bounds.maxx, node.x),
                    miny: Math.min(bounds.miny, node.y), maxy: Math.max(bounds.maxy, node.y),
                    maxr: Math.max(bounds.maxr, node.size)};
        }, {minx: Infinity, miny: Infinity, maxx: -Infinity, maxy: -Infinity, maxr: -Infinity});
        var SVGCanvas = document.createElementNS('http://www.w3.org/2000/svg','svg');
        SVGCanvas.setAttribute('version', '1.1');
        SVGCanvas.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
        SVGCanvas.setAttribute('width',  bounds.maxx - bounds.minx + 2 * bounds.maxr);
        SVGCanvas.setAttribute('height', bounds.maxy - bounds.miny + 2 * bounds.maxr);
        shownEdges.get().map( function (edge) {
            return {from: shownNodes.get(edge.from), to: shownNodes.get(edge.to),
                    width: edge.width, color: edge.color.color, label: edge.label};
        }).forEach( function (obj) {
            SVGCanvas.appendChild(new Edge(obj.from.x - bounds.minx + bounds.maxr,
                                           obj.from.y - bounds.miny + bounds.maxr,
                                           obj.to.x   - bounds.minx + bounds.maxr,
                                           obj.to.y   - bounds.miny + bounds.maxr,
                                           obj.width,    obj.color,   obj.label  ).edge);
        });
        nodeArr.forEach( function (node) {
            SVGCanvas.appendChild(new Node(node.x - bounds.minx + bounds.maxr,
                                           node.y - bounds.miny + bounds.maxr,
                                           node.size, node.color.background).node);
        });
        // Return text version of complete SVG image
        return document.createElement('div').appendChild(SVGCanvas).parentNode.innerHTML;
    }

    var img = btoa(export_SVG());
    var link = document.createElement('a');
    link.setAttribute('href-lang', 'image/svg+xml');
    link.setAttribute('href', 'data:image/svg+xml;base64,\n'+img);
    link.setAttribute('title', 'netloc_draw.svg');
    link.appendChild(document.createTextNode("image"));
    document.getElementById('imageLink').appendChild(link);
}
