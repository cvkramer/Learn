window.addEventListener('load', function(event) {
    console.log('onload');
    $.ajax({
        url: 'http://localhost:8000/Data/data.JSON',
        method: 'get',
        success: loadData
    });

    function loadData(json) {
        // read the json blob and do stuff
        console.log('data is loaded!');
        console.log(json);
        var rootDiv = document.getElementById('root');
        //rootDiv.innerHTML = json.A;
        barChart(json.A);
    }

    function plotData(data) {
        // change string (from CSV) into number format
        /*
        data.forEach(function(d) {
            console.log(d);
        });
        */
        var margin = {top: 20, right: 20, bottom: 30, left: 40};
        var width = 960 - margin.left - margin.right;
        var height = 500 - margin.top - margin.bottom;

        var xValue = function(d) { return d;}; // data -> value
        var xScale = d3.scale.linear().range([0, width]); // value -> display
        var xMap = function(d) { return xScale(xValue(d));}; // data -> display
        var xAxis = d3.svg.axis().scale(xScale).orient("bottom");

        // setup y
        var yValue = function(d) { return d;}; // data -> value
        var yScale = d3.scale.linear().range([height, 0]); // value -> display
        var yMap = function(d) { return yScale(yValue(d));}; // data -> display
        var yAxis = d3.svg.axis().scale(yScale).orient("left");

        // add the graph canvas to the body of the webpage
        var svg = d3.select("body").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis)
        .append("text")
        .attr("class", "label")
        .attr("x", width)
        .attr("y", -6)
        .style("text-anchor", "end")
        .text("index");

        // y-axis
        svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
        .append("text")
        .attr("class", "label")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("A");

        // draw dots
        svg.selectAll(".dot")
        .data(data)
        .enter().append("circle")
        .attr("class", "dot")
        .attr("r", 3.5)
        .attr("cx", xMap)
        .attr("cy", yMap)
        .style("fill", "blue");
    }
    
    
    function plotBarChart(data) {
        var data = [4, 8, 15, 16, 23, 42];
        var width = 420,
            barHeight = 20;
        
        // Define Graphing Region
        var margin = {top: 20, right: 20, bottom: 30, left: 40};
        var width = 960 - margin.left - margin.right;
        var height = 500 - margin.top - margin.bottom;

        var xValue = function(d) { return d;}; // data -> value
        var xScale = d3.scale.linear().domain([0,50]).range([0, width]); // value -> display
        var xMap = function(d) { console.log(d); return xScale(xValue(d));}; // data -> display
        var xAxis = d3.svg.axis().scale(xScale).orient("bottom");

        // setup y
        var yValue = function(d) { return d;}; // data -> value
        var yScale = d3.scale.linear().domain([0,50]).range([height, 0]); // value -> display
        var yMap = function(d) { return yScale(yValue(d));}; // data -> display
        var yAxis = d3.svg.axis().scale(yScale).orient("left");

        // add the graph canvas to the body of the webpage
        var svg = d3.select("body").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        // x-axis
        svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis)
        .append("text")
        .attr("class", "label")
        .attr("x", width)
        .attr("y", -6)
        .style("text-anchor", "end")
        .text("index");

        // y-axis
        svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
        .append("text")
        .attr("class", "label")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("A");

        // draw dots
        bar = svg.selectAll("g")
            .data(data)
          .enter().append("g")
            .attr("transform", function(d, i) { return "translate(0," + i * barHeight + ")"; });
        
        bar.append("rect")
            .attr("width", xMap)
            .attr("height", barHeight - 1);
    }
    
    function barChart(data){
        console.log(data)
        
        var width = 420,
            height = 500,
            barHeight = 20;
        
        var svg = d3.select("body").append("svg")
            .attr("width", width)
            .attr("height", barHeight * data.length);
        
        var x = d3.scale.linear()
            .domain([0, d3.max(data)])
            .range([0, width]);
        console.log(x)
        
        var bar = svg.selectAll("g")
            .data(data)
          .enter().append("g")
            .attr("transform", function(d, i) { return "translate(0," + i * barHeight + ")"; });
        
        bar.append("rect")
            .attr("width", x)
            .attr("height", barHeight - 1);
        
        bar.append("text")
            .attr("x", function(d) { return x(d) - 3; })
            .attr("y", barHeight / 2)
            .attr("dy", ".35em")
            .text(function(d) { return d; });
            
        console.log(bar)
    }
    
});


