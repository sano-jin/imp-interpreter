"use strict";

/* This should be the only public function. 
 * Others should be hided (though I haven't done yet).
 */
const renderProofTree = ( div, json ) => {
    console.log( div, json );
    div.textContent = "";
    const wrapperDiv = createDiv( "--proof-tree-wrapper" );
    const root = wrapperDiv.appendChild( getNode( json ) )
    div.appendChild( wrapperDiv );
    redrawBars( root );
}

/* Some helper functions */
const createDiv = ( className ) => {
    const div = document.createElement("div");
    if ( className ) div.classList.add( className );
    return div;
}

const createTextDiv = ( str, className ) => {
    const div = createDiv( className );
    div.appendChild( document.createTextNode( str ) );
    return div;
}

/* Gets the Nodes of the proof tree.
 * This can be done at any time.
 * However the bar and the rule name will not fit compactly.
 * Therefore you must call redrawBars after this.
*/
const getNode = ( json ) => { 
    const nodeDiv = createDiv( "--proof-tree-node" );
    const consequentDiv = createTextDiv( json.consequent, "--proof-tree-consequent" );
    if ( json.rule !== undefined ) {
	const antecedentsDiv = createDiv( "--proof-tree-antecedents" );
	json.antecedents
	    .map( getNode )
	    .flatMap( e => [ createDiv( "--proof-tree-padding" ), e ] ).slice( 1 )
	    .map( aDiv => antecedentsDiv.appendChild( aDiv ) );
	const bar = createDiv( "--proof-tree-bar" );
	bar.appendChild( createTextDiv ( json.rule, "--proof-tree-rule" ) );
	[ antecedentsDiv, 
	  bar
	].map( child => nodeDiv.appendChild( child ) );
    }
    nodeDiv.appendChild( consequentDiv );
    return nodeDiv; 
}

/* Redraw bars and rule names.
 * This process MUST be done AFTER css is applied,
 * since it is calculating the size of the text.
*/
const redrawBars = ( root ) => {
    console.log( "page loading MUST be finished before starting this" );
    
    const treeWalker = document.createTreeWalker(
	root,
	NodeFilter.SHOW_ELEMENT,
	{ acceptNode: ( node ) => {
	    if ( node.classList.contains( "--proof-tree-node" ) )
		return NodeFilter.FILTER_ACCEPT;
	    else
		return NodeFilter.FILTER_SKIP;
	}},
	false
    );
    
    let currentNode = treeWalker.currentNode;
    while( currentNode ) {
	setBar( currentNode );
	currentNode = treeWalker.nextNode();
    }
}

/* Redraw bar (and the rule).
 * This process MUST be done AFTER css is applied,
 * since it is calculating the size of the text.
*/
const setBar = ( node ) => {
    if ( node.children.length === 3 ) {
	const antecedents = node.children[ 0 ];
	if ( antecedents.children.length === 0 ) return;
	const bar = node.children[ 1 ];
	const consequent = node.children[ 2 ];
	const left = antecedents.firstChild.lastChild;
	const right = antecedents.lastChild.lastChild;
	const leftOffset = Math.min( left.offsetLeft, consequent.offsetLeft );
	bar.style.left = leftOffset - 10 + "px";
	const rightOffset = Math.max( antecedents.lastChild.offsetLeft + right.offsetLeft + right.offsetWidth,
				      consequent.offsetLeft + consequent.offsetWidth );
	const width = rightOffset - leftOffset;
	bar.style.width = width + 20 + "px";	
    }
}


			       
