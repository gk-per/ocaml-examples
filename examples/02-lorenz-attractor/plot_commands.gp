
      set term png
      set output 'lorenz_attractor.png'
      set title 'Lorenz Attractor'
      set xlabel 'X'
      set ylabel 'Y'
      set zlabel 'Z'
      splot 'lorenz_data.dat' with lines notitle
    