for nPoints in `seq 1 $1`
do
    fileName="$nPoints""_points.smt2";
    
    # information of the problem
    echo "(set-logic QF_NRA)" > $fileName;
    echo "(set-info :source | x_i^2 + y_i^2 + z_i^2 = 1 and (x_i - x_j)^2 + (y_i - y_j)^2 + (z_i - z_j)^2 \geq 1 if i \ neq j for 1 \leq i \leq n |)" >> $fileName;
    
    # declare variables
    for point in `seq 1 $nPoints`
    do
      echo "(declare-fun x_"$point" () Real)" >> $fileName
      echo "(declare-fun y_"$point" () Real)" >> $fileName
      echo "(declare-fun z_"$point" () Real)" >> $fileName
    done
    
    # declare APIs
    # First, the equations of the circles are declared
    for point in `seq 1 $nPoints`
    do
      echo "(assert (= (+ (* x_"$point" x_"$point") (+ (* y_"$point" y_"$point") (* z_"$point" z_"$point"))) 1.))" >> $fileName
    done
    # Next, the inequations are declared:
    temp=`expr $nPoints - 1`
    for point in `seq 1 $temp`
    do
      nextTemp=`expr $point + 1`
      for nextPoint in `seq $nextTemp $nPoints`
      do
        echo "(assert (>= (+ (* (- x_"$point" x_"$nextPoint") (- x_"$point" x_"$nextPoint")) ( + (* (- y_"$point" y_"$nextPoint") (- y_"$point" y_"$nextPoint")) (* (- z_"$point" z_"$nextPoint") (- z_"$point" z_"$nextPoint")))) 1.))" >> $fileName
      done
    done
    
    # add goal
    echo "(check-sat)" >> $fileName;
done
