
document.addEventListener("DOMContentLoaded", function (event) {
    Chart.defaults.global.defaultColor = '#48BEFF'
    for (var i = 0; i < 8; i++) {
        var ctx = document.getElementById('gpu-canvas_1_' + i).getContext('2d');
        new Chart(ctx, {
            type: 'line',
            data: {
                labels: [0, 1, 2, 3, 4, 5],
                datasets: [{
                    label: 'GPU Load',
                    data: [0.1, 0.34, 0.98, 0.64, 0.88, 1.00],
                    borderColor: '#48BEFF',
                    backgroundColor: '#48BEFF10',
                    borderWidth: 1
                }]
            },
            options: {
                scales: {
                    yAxes: [{
                        ticks: {
                            beginAtZero: true
                        }
                    }]
                },
                title: {
                    display: true,
                    text: 'Titan XP'
                }
            }
        });
    }
});