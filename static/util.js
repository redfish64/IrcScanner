

/**
 * Return 0 <= i <= array.length such that !pred(array[i - 1]) && pred(array[i]).
 */
function binarySearch(array, pred) {
    return binarySearch2(array, pred, -1,array.length);
}

function binarySearch2(array, pred,lo,hi) {
    while (1 + lo !== hi) {
        const mi = lo + ((hi - lo) >> 1);
        if (pred(array[mi])) {
            hi = mi;
        } else {
            lo = mi;
        }
    }
    return hi;
}

function binarySearchFind(array, pred) {
    return array[binarySearch2(array, pred, -1,array.length)];
}




function isInView(elem) {

    var docViewTop = $(window).scrollTop();
    var docViewBottom = docViewTop + $(window).height();

    var elemTop = $(elem).offset().top;
    var elemBottom = elemTop + $(elem).height();

    return ((elemBottom >= docViewTop) && (elemTop <= docViewBottom));
}


function isBelowBottomOfScreen(elem) {

    var docViewTop = $(window).scrollTop();
    var docViewBottom = docViewTop + $(window).height();

    var elemTop = $(elem).offset().top;

    return (elemTop >= docViewBottom);
}

function isBelowTopOfScreen(elem) {

    var docViewTop = $(window).scrollTop();

    var elemTop = $(elem).offset().top;
    var elemBottom = elemTop + $(elem).height();

    return (elemBottom >= docViewTop);
}

function getElementsInView(elems) {
    var elemArray = elems.toArray();
    var startIndex = binarySearch(elemArray, isBelowTopOfScreen);
    var endIndex = binarySearch2(elemArray, isBelowBottomOfScreen, startIndex, elemArray.length);
    return elemArray.slice(startIndex, endIndex);
}


