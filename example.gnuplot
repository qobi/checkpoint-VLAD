set terminal pdf
set linetype 1 linewidth 2 linecolor rgbcolor "red" pointtype 1
set linetype 2 linewidth 2 linecolor rgbcolor "green" pointtype 2
set linetype 3 linewidth 2 linecolor rgbcolor "blue" pointtype 3
set linetype 4 linewidth 2 linecolor rgbcolor "violet" pointtype 4
set linetype 5 linewidth 2 linecolor rgbcolor "orange" pointtype 5
set linetype 6 linewidth 2 linecolor rgbcolor "cyan" pointtype 6
set linetype 7 linewidth 2 linecolor rgbcolor "black" pointtype 7
set output 'example-space.pdf'
set title 'space'
set xlabel 'l'
set ylabel 'Maximum resident set size (kbytes)'
plot 'a-space.data' title 'Tapenade no checkpointing'\
     with linespoints,\
     'b-space.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints,\
     'c-space.data' title 'checkpointVLAD no checkpointing'\
     with linespoints,\
     'd-space.data' title 'checkpointVLAD binary bisection checkpointing'\
     with linespoints
set output 'example-time.pdf'
set title 'time'
set xlabel 'l'
set ylabel 'User time (seconds)'
plot 'a-time.data' title 'Tapenade no checkpointing'\
     with linespoints,\
     'b-time.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints,\
     'c-time.data' title 'checkpointVLAD no checkpointing'\
     with linespoints,\
     'd-time.data' title 'checkpointVLAD binary bisection checkpointing'\
     with linespoints
set output 'Tapenade-space.pdf'
set title 'space'
set xlabel 'l'
set ylabel 'Maximum resident set size (kbytes)'
plot 'a-space.data' title 'Tapenade no checkpointing'\
     with linespoints,\
     'b-space.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints,\
     'e-space.data' title 'O(l)'\
     with linespoints,\
     'f-space.data' title 'O(l)'\
     with linespoints
set output 'Tapenade-time.pdf'
set title 'time'
set xlabel 'l'
set ylabel 'User time (seconds)'
plot 'a-time.data' title 'Tapenade no checkpointing'\
     with linespoints,\
     'b-time.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints,\
     'e-time.data' title 'O(l)'\
     with linespoints,\
     'f-time.data' title 'O(l**(1+1/40))'\
     with linespoints
set output 'checkpointVLAD-space.pdf'
set title 'space'
set xlabel 'l'
set ylabel 'Maximum resident set size (kbytes)'
plot 'c-space.data' title 'checkpointVLAD no checkpointing'\
     with linespoints,\
     'd-space.data' title 'checkpointVLAD binary bisection checkpointing'\
     with linespoints,\
     'g-space.data' title 'O(l)'\
     with linespoints,\
     'h-space.data' title 'O(log l)'\
     with linespoints
set output 'checkpointVLAD-time.pdf'
set title 'time'
set xlabel 'l'
set ylabel 'User time (seconds)'
plot 'c-time.data' title 'checkpointVLAD no checkpointing'\
     with linespoints,\
     'd-time.data' title 'checkpointVLAD binary bisection checkpointing'\
     with linespoints,\
     'g-time.data' title 'O(l)'\
     with linespoints,\
     'h-time.data' title 'O(l log l)'\
     with linespoints
set output 'compare-space.pdf'
set title 'space'
set xlabel 'l'
set ylabel 'Maximum resident set size (kbytes)'
plot 'b-space.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints,\
     'd-space.data' title 'checkpointVLAD binary bisection checkpointing'\
     with linespoints
set output 'Tapenade-checkpointing-space.pdf'
set title 'space'
set xlabel 'l'
set ylabel 'Maximum resident set size (kbytes)'
plot 'b-space.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints
set output 'Tapenade-checkpointing-time.pdf'
set title 'time'
set xlabel 'l'
set ylabel 'User time (seconds)'
plot 'b-time.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints
set output 'small-space.pdf'
set title 'space'
set xlabel 'l'
set ylabel 'Maximum resident set size (kbytes)'
plot 'a-space.data' title 'Tapenade no checkpointing'\
     with linespoints,\
     'b-space.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints,\
     'd-space.data' title 'checkpointVLAD binary bisection checkpointing'\
     with linespoints
set output 'small-time.pdf'
set title 'time'
set xlabel 'l'
set ylabel 'User time (seconds)'
plot 'a-time.data' title 'Tapenade no checkpointing'\
     with linespoints,\
     'b-time.data' title 'Tapenade treeverse binomial checkpointing'\
     with linespoints
