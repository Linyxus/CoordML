
document.addEventListener("DOMContentLoaded", function (event) {
    Chart.defaults.global.defaultColor = '#48BEFF'
    for (var i = 0; i < 8; i++) {
        var ctx = document.getElementById('gpu-canvas_1_' + i).getContext('2d');
        new Chart(ctx, {
            type: 'line',
            data: {
                labels: [0, 1, 2, 3, 4, 5],
                datasets: [
                    {
                        label: 'GPU Load',
                        data: [0.1, 0.34, 0.98, 0.64, 0.88, 1.00],
                        borderColor: '#48BEFF',
                        backgroundColor: '#48BEFF10',
                        borderWidth: 1,
                        yAxisID: 'left-y-axis'
                    },
                    {
                        label: 'GPU Mem',
                        data: [21.3, 3.2, 4.3, 8.3, 12.9, 31.1],
                        borderColor: '#B6174B',
                        backgroundColor: '#B6174B10',
                        borderWidth: 1,
                        yAxisID: 'right-y-axis'
                    }
                ]
            },
            options: {
                scales: {
                    ticks: {
                        beginAtZero: true
                    },
                    yAxes: [{
                        id: 'left-y-axis',
                        type: 'linear',
                        position: 'left',
                        ticks: {
                            min: 0.0,
                            max: 1.0
                        }
                    }, {
                        id: 'right-y-axis',
                        type: 'linear',
                        position: 'right',
                        ticks: {
                            min: 0.0,
                            max: 32.0,
                            stepSize: 16
                        }
                    }],
                },
                title: {
                    display: true,
                    text: 'Titan XP',
                    position: 'left'
                }
            },
        });
    }
});