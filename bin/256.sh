#!/bin/sh

## system color (0 - 15)
for i in `seq 0 1`; do
    for j in `seq 0 7`; do
        c=$(($i * 8 + $j))
        printf "\x1b[48;5;${c}m %2d \x1b[0m" $c
    done
    echo
done

echo

## 6x6x6 pallet (16 - 231)
for i in `seq 0 5`; do
    for j in `seq 0 5`; do
        for k in `seq 0 5`; do
            c=$(($i * 6 + $j * 36 + $k + 16))
            printf "\x1b[48;5;${c}m %3d\x1b[0m" $c
        done
        /bin/echo -n ' '
    done
    echo
done

echo

## gray (232 - 255) with white foreground
for c in `seq 232 255`; do
    printf "\x1b[48;5;${c}m %3d\x1b[0m" $c
done
echo
## gray (232 - 255) with black foreground
for c in `seq 232 255`; do
    printf "\x1b[30;48;5;${c}m %3d\x1b[0m" $c
done