const address = 'http://127.0.0.1:8042';

let history = [];

function updateView(data) {
    let monitoring = document.getElementById('monitoring');

    let cpu = '<h4>CPU</h4>';

    cpu += 'Load: ' + data.cpu.load + '%<br>';

    cpu += data.cpu.cores.reduce(function(acc, core, index) {
	return acc + '<li>Core #' + index + ' ' + core + '%</li>'
    }, '<ul>') + '</ul>';

    cpu += 'Switch contexts: ' + data.cpu.switchContext + '<br>';
    cpu += 'Procs running: ' + data.cpu.procsRunning + '<br>';
    cpu += 'Procs blocked: ' + data.cpu.procsBlocked + '<br>';
    cpu += 'Switch contexts: ' + data.cpu.switchContext + '<br>';

    let mem = '<h4>MEMORY</h4>';

    mem += 'Total: ' + data.mem.total + 'ko<br>';
    mem += 'Free: ' + data.mem.free + 'ko<br>';
    mem += 'Occupied: ' + data.mem.occupied + 'ko<br>';
    mem += 'Available: ' + data.mem.available + 'ko<br>';
    mem += 'Swap total: ' + data.mem.swapTotal + 'ko<br>';
    mem += 'Swap free: ' + data.mem.swapFree + 'ko<br>';
    mem += 'Swap occupied: ' + data.mem.swapOccupied + 'ko<br>';

    let net = '<h4>NETWORK</h4>';

    net += data.net.reduce(function(acc, interfac) {
	return acc + '<li>' + interfac.name + ' : ' +
	    interfac.download + ' ko/s / ' + interfac.upload  + ' ko/s</li>';
    }, '<ul>') + '</ul>';

    monitoring.innerHTML = cpu + mem + net;
}

function drawChart(monitor) {
    let labels = monitor.cpu.cores.map(function(load, index) {
	return 'Core #' + index;
    });

    let data = monitor.cpu.cores;

    let ctx = document.getElementById("chart").getContext('2d');
    let myChart = new Chart(ctx, {
	type: 'line',
	monitor: {
            labels: labels,
            monitorsets: [{
		label: 'Percent load Core',
		data: data,
		backgroundColor: [
                    'rgba(255, 99, 132, 0.2)',
                    'rgba(54, 162, 235, 0.2)',
                    'rgba(255, 206, 86, 0.2)',
                    'rgba(75, 192, 192, 0.2)',
                    'rgba(153, 102, 255, 0.2)',
                    'rgba(255, 159, 64, 0.2)'
		],
		borderColor: [
                    'rgba(255,99,132,1)',
                    'rgba(54, 162, 235, 1)',
                    'rgba(255, 206, 86, 1)',
                    'rgba(75, 192, 192, 1)',
                    'rgba(153, 102, 255, 1)',
                    'rgba(255, 159, 64, 1)'
		],
		borderWidth: 1
            }]
	},
	
	options: {
	    animation: false,
            scales: {
		yAxes: [{
                    ticks: {
			beginAtZero:true
                    }
		}]
            }
	}
    });
}

window.onload = function() {
    let sse = new EventSource(address);
    
    sse.onopen = function(event) {
	console.log('Connected to EventSource ' + address, '\n');
    };

    sse.onmessage = function(event) {
	let monitor;
	try { monitor = JSON.parse(event.data); } catch (err) {
	    console.log('JSON.parse failed: ', err);
	    return;
	}

	updateView(monitor);
	history.push(monitor);
	drawChart(monitor);
    };

    sse.onerror = function(err) {
	console.log('SSE failed: ', err);
    };
}
