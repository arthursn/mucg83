# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from io import StringIO
from subprocess import Popen, PIPE
import argparse


def parse_temperature(key, line):
    value = None
    if key in line:
        arr = line.split('=')
        value = arr[-1].replace('C', '').strip()
        value = float(value)
    return value


def parse_stdout(stdout):
    separator = ('----------------------------------------------------------------'
                 '---------------------------------------------------------------')

    arr = stdout.split(separator)
    # Table with TTT info is in the 3rd block (index 2)
    tablestr = arr[2].strip()

    Ws = None    # WIDMANSTATTEN FERRITE START TEMPERATURE
    Bs_g = None  # GROWTH LIMITED BAINITE START TEMPERATURE
    Bs_n = None  # NUCLEATION LIMITED BAINITE START TEMP
    Ms = None    # MARTENSITE START TEMPERATURE

    read_table = True
    table = []
    for idx, line in enumerate(tablestr.split('\n')):
        line = line.strip()

        if not line:  # end of table has been reached if line is empty
            read_table = False

        if read_table:
            if idx > 0:  # if not header
                table.append(line.replace('D', 'E'))  # e.g, 0.67D+03 -> 0.67E+03
            else:
                table.append(line)  # appends the header
        else:
            if not Ws:
                Ws = parse_temperature('WIDMANSTATTEN FERRITE START TEMPERATURE', line)
            if not Bs_g:
                Bs_g = parse_temperature('GROWTH LIMITED BAINITE START TEMPERATURE', line)
            if not Bs_n:
                Bs_n = parse_temperature('NUCLEATION LIMITED BAINITE START TEMP', line)
            if not Ms:
                Ms = parse_temperature('MARTENSITE START TEMPERATURE', line)

    # Uses pandas to convert table (string) into pandas DataFrame
    df = pd.read_csv(StringIO('\n'.join(table)), delim_whitespace=True)

    df['SHEAR_CTEMP'] = df['CTEMP']
    if Bs_n:
        # assigns nan to SHEAR_CTEMP when SHEAR_CTEMP is higher than Bs
        df['SHEAR_CTEMP'].where(cond=df['SHEAR_CTEMP'] <= Bs_n, other=np.nan, inplace=True)

    return df, Ws, Bs_g, Bs_n, Ms


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--C', type=float, default=0., help='Carbon wt.%')
    parser.add_argument('--Si', type=float, default=0., help='Silicon wt.%')
    parser.add_argument('--Mn', type=float, default=0., help='Manganese wt.%')
    parser.add_argument('--Ni', type=float, default=0., help='Nickel wt.%')
    parser.add_argument('--Mo', type=float, default=0., help='Molybdenum wt.%')
    parser.add_argument('--Cr', type=float, default=0., help='Chromium wt.%')
    parser.add_argument('--V', type=float, default=0., help='Vanadium wt.%')
    parser.add_argument('--Co', type=float, default=0., help='Cobalt wt.%')
    parser.add_argument('--Cu', type=float, default=0., help='Copper wt.%')
    parser.add_argument('--Al', type=float, default=0., help='Aluminium wt.%')
    parser.add_argument('--W', type=float, default=0., help='Tungsten wt.%')

    # Calls mucg83
    cmd = './mucg83'
    proc = Popen(cmd,
                 stdin=PIPE,
                 stdout=PIPE,
                 stderr=PIPE)

    composition = vars(parser.parse_args())
    args = ('1\n'
            '{C}\n'     # Carbon wt.% ?
            '{Si}\n'    # Silicon wt.% ?
            '{Mn}\n'    # Manganese wt.% ?
            '{Ni}\n'    # Nickel wt.% ?
            '{Mo}\n'    # Molybdenum wt.% ?
            '{Cr}\n'    # Chromium wt.% ?
            '{V}\n'     # Vanadium wt.% ?
            '{Co}\n'    # Cobalt wt.% ?
            '{Cu}\n'    # Copper wt.% ?
            '{Al}\n'    # Aluminium wt.% ?
            '{W}\n'     # Tungsten wt.% ?
            '0').format(**composition)

    # Passes compositions to mucg83 and reads output
    # Important information is stored in stdout
    stdout, stderr = proc.communicate(args.encode())

    try:
        proc.kill()
    except:
        pass

    # Parse stdout into pandas DataFrame df and critical temperatures
    # Ws, Bs_g, Bs_n, and Ms
    df, Ws, Bs_g, Bs_n, Ms = parse_stdout(stdout.decode())

    # Plot TTT
    fig, ax = plt.subplots()

    ax.plot(df['DIFFT'], df['CTEMP'], 'k-', label='Ferrite + Pearlite')
    ax.plot(df['SHEART'], df['SHEAR_CTEMP'], 'r-', label='Bainite')

    ax.set_xscale('log')
    ax.set_xlim(1e-2, 1e6)
    # ax.set_ylim(100, 900)
    ax.set_xlabel('Time (s)')
    ax.set_ylabel(u'Temperature (°C)')

    title = ['{}{}'.format(v, k) for k, v in composition.items() if v > 0]
    title.insert(0, 'Fe')
    ax.set_title('-'.join(title))

    if Bs_n:
        print('Bainite start temperature:', Bs_n, 'oC')
        ax.axhline(Bs_n, color='r', ls='--')
        ax.text(ax.get_xlim()[0]*1.5, Bs_n + 10, 'Bainite start', color='r')
    if Ms:
        print('Martensite start temperature:', Ms, 'oC')
        ax.axhline(Ms, color='b', ls='--')
        ax.text(ax.get_xlim()[0]*1.5, Ms + 10, 'Martensite start', color='b')

    ax.legend()

    plt.show()
