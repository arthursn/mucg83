# About

[MUCG83](https://www.phase-trans.msm.cam.ac.uk/map/steel/programs/mucg83.html) is a software developed by Mathew Peet and H.K.D.H. Bhadeshia from the University of Cambridge for modelling the thermodynamics and kinetics of solid-state phase transformations in steels. The original software runs in a shell (command prompt) and the compositions have to be provided by manual input of the user, while the output is printed in the shell. The TTT diagrams can be plotted in a external software. This process can be quite boring. In order to solve this, the python script `mucg83_python_interface.py` in this repository provides a simple interface for generating TTT plots from the calculations performed in mucg83.

In addition, the script features an option to calculate the CCT diagrams from the TTT curves by using Scheil's method (1935), based on the assumption that the continuous cooling curve is a combination of sufficiently large number of isothermal reaction steps. The rule can be justified if reaction rate solely depends on volume fraction and
temperature. Disclaimer: The CCT curves generated using this code, specially those corresponding to the bainite reaction, might not truthfully represent the correct behavior observed experimentally. These CCT curves should only be used for guidance.

Feel free to edit the code to, for instance, automatize the generation of TTT diagrams! The two main functions in the script are `run_mucg83(cmd, **composition)` and `parse_stdout(stdout)`. The former runs the mucg83 simulation and stores the output in two strings `stdout` and `stderr`. The important information is in `stdout`. `parse_stdout(stdout)` then can be used to parse the string `stdout`, returning a pandas DataFrame and the critical temperatures calculated by mucg83.

# Dependencies

The python script runs using the non standard python libraries `argparse`, `numpy`, `matplotlib`, `pandas`, and `scipy`. Install them from the [Python Package Index](https://pypi.org/):

```bash
pip install numpy matplotlib pandas argparse scipy
```

# Usage

```bash
usage: mucg83_python_interface.py [-h] [-C C] [-Si SI] [-Mn MN] [-Ni NI]
                                  [-Mo MO] [-Cr CR] [-V V] [-Co CO] [-Cu CU]
                                  [-Al AL] [-W W] [--cmd CMD] [-p PLOT]
                                  [-T0 T0]
```

# Examples

Plot TTT diagram:

```bash
python mucg83_python_interface.py -C 0.1 -Mn 1
```

This will prompt a matplotlib window with the following TTT diagram:

![Fe-0.1%C-1%Mn TTT](img/Fe-01C-1Mn_TTT.png)

Plot CCT diagram:

```bash
python mucg83_python_interface.py -C 0.1 -Mn 1 --plot CCT
```

![Fe-0.1%C-1%Mn TTT](img/Fe-01C-1Mn_CCT.png)

Plot TTT and CCT diagrams:

```bash
python mucg83_python_interface.py -C 0.1 -Mn 1 --plot both
```

![Fe-0.1%C-1%Mn TTT](img/Fe-01C-1Mn_both.png)
