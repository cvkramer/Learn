window.onload = function(e) {
    var rootDiv = document.getElementById('root');
    $.ajax({
        url: '<data url goes here>',
        method: 'get',
        success: loadData
    });
}

function loadData(json) {
    // read the json blob and do stuff
}
