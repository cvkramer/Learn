window.addEventListener('load', function(event) {
    console.log('onload');
    $.ajax({
        url: 'http://localhost:8000/data.JSON',
        crossDomain: true,
        method: 'get',
        success: loadData
    });

    function loadData(json) {
        // read the json blob and do stuff
        console.log('data is loaded!');
        console.log(json);
        //var rootDiv = document.getElementById('root');
    }
});


