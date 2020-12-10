async function scatterplot(){


    // 1. Acces Data
    const dataset = await d3.csv('./data.csv')

    dataset.forEach( d => {
        d.years_leaking = +d.years_leaking; // convert to number
        d.years_lost = +d.years_lost;
        });

    const xAccessor = d => d.years_leaking
    const yAccessor = d => d.years_lost
    


    //2. Create Chart Dimensions
    let dimensions = {
        width: 550,
        height: 550,
        margin:  {           
            top: 20,
            right: 10,
            bottom: 50,
            left: 50
        }
    };

    dimensions.boundedWidth = dimensions.width - dimensions.margin.left - dimensions.margin.right
    dimensions.boundedHeight = dimensions.height - dimensions.margin.top - dimensions.margin.bottom

    // 3. Draw Canvas
    const wrapper = d3.select('#wrapper1')
        .append('svg')
            .attr('width', dimensions.width)
            .attr('height', dimensions.height)

    const bounds = wrapper.append('g')
        .style(
            'transform',
            // the left margin pushes our bounds to the right
            // the top margin pushes our bounds down
            `translate(${ dimensions.margin.left}px,
                    ${dimensions.margin.top}px)`
        )

    // Create scales
    const xScale = d3.scaleLinear()
    .domain(d3.extent(dataset, xAccessor))
    .range([0, dimensions.boundedWidth])

const yScale = d3.scaleLinear()
    .domain(d3.extent(dataset, yAccessor))
    .range([dimensions.boundedWidth -15,0])
    // 4. Draw Data
    const dots = bounds.selectAll('circle')
    .data(dataset)
    .enter().append('circle')
      .attr('cx', d => xScale(xAccessor(d)))
      .attr('cy', d => yScale(yAccessor(d)) + Math.random() * 15)
      .attr('r',4)
      .attr('class','dots')


    //5.Draw peripherals
    const xAxisGenerator = d3.axisBottom()
        .scale(xScale)
        .tickValues([0,5,10,15,20,25,30,35,40])
    const xAxis = bounds.append('g')
        .call(xAxisGenerator)
        .attr('color','#444444')
        .style('transform',`translateY(${dimensions.boundedHeight + 5}px)`)
        // .call(g => g.select(".domain").remove())

    const xAxislabel = xAxis.append('text') 
        // We want it horizontally centered
        .attr('x', dimensions.boundedWidth / 2)
        // we want it slightly abobe the bottom of the chart
        .attr('y', dimensions.margin.bottom - 10)
        .html("Leaking Years")
        .attr('class','x-axis-label')
        

    const yAxisGenerator = d3.axisLeft()
        .scale(yScale)
        .tickValues([0,2,4,6,8,10,12,14])

    const yAxis = bounds.append('g')
        .call(yAxisGenerator)
            .attr('color','#444444')
            // .call(g => g.select(".domain").remove())

    const yAxisLabel = yAxis.append('text')
        .attr('x', -dimensions.boundedHeight / 2.2)
        // we want the y axis in the middle of the y-axis
        .attr('y', -dimensions.margin.left + 14)
        .attr('class','y-axis-label')
        .style('transform','rotate(-90deg')
        .text('Years Lost')
        .style('text-anchor','middle')



};

scatterplot();

async function heat(){


    // 1. Acces Data
    const dataset = await d3.csv('./data.csv')

    dataset.forEach( (d,i) => {
        d.years_leaking = +d.years_leaking; // convert to number
        d.years_lost = +d.years_lost;
        d.x = +d.x;
        d.y = +d.y;
        });
    
    const colorAccessor = d => d.years_lost
    const sizeAccessor = d => d.years_leaking
    const xAccessor = d => d.x
    const yAccessor = d => d.y




    //2. Create Chart Dimensions
    let dimensions = {
        width: 1000,
        height: 1200,
        margin:  {           
            top: 20,
            right: 10,
            bottom: 0,
            left: 10
        }
    };

    dimensions.boundedWidth = dimensions.width - dimensions.margin.left - dimensions.margin.right
    dimensions.boundedHeight = dimensions.height - dimensions.margin.top - dimensions.margin.bottom

    // 3. Draw Canvas
    const wrapper = d3.select('#wrapper2')
        .append('svg')
            .attr('width', dimensions.width)
            .attr('height', dimensions.height)

    const bounds = wrapper.append('g')
        .style(
            'transform',
            // the left margin pushes our bounds to the right
            // the top margin pushes our bounds down
            `translate(${ dimensions.margin.left}px,
                    ${dimensions.margin.top}px)`
        )

    // Scales
    const xScale = d3.scaleLinear()
        .domain(d3.extent(dataset, xAccessor))
        .range([0, dimensions.boundedWidth])

    const yScale = d3.scaleLinear()
        .domain(d3.extent(dataset, yAccessor))
        .range([0, dimensions.boundedWidth])
    const sizeScale = d3.scaleLinear()
        .domain(d3.extent(dataset,sizeAccessor))
        .range([3,13])
    const colorScale = d3.scaleLinear()
        .domain(d3.extent(dataset,colorAccessor))
        .range(['#d8b1d4','#800080'])


    const dots = bounds.selectAll('circle')
        .data(dataset)
        .enter().append('circle')
            .attr('cx', d => xScale(xAccessor(d)))
            .attr('cy', d => yScale(yAccessor(d)))
            .attr('r', d => sizeScale(sizeAccessor(d)))
            .attr('fill', d => colorScale(colorAccessor(d)))
            .attr('class','plot2-circle')


    bounds.selectAll('.plot2-circle')
        .on('mouseenter',onMouseEnter)
        .on('mouseleave',onMouseLeave)

    const tooltip = d3.select('#tooltip')
    function onMouseEnter(event,datum){
        const index = d3.selectAll('circle').nodes().indexOf(this)

        // Positioning the tooltip

        tooltip
            .style("top", (event.pageY)+"px")
            .style("left",(event.pageX)+"px")
        
        const age = datum.age
        const country = datum.country
        const years = datum.years_leaking
        const lost = datum.years_lost
        const format = d3.format(".2f")
        tooltip.select('#age')
            .text(age)
        tooltip.select('#country')
            .text(country)
        tooltip.select('#years')
            .text(years)
        tooltip.select('#lost')
            .text(format(lost))


        tooltip
            .style('opacity',1)
    }

    function onMouseLeave(event,datum){
        tooltip
        .style('opacity',0)
    }
}


heat()