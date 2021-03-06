# -*- coding: utf-8 -*-

from __future__ import print_function
import platform
import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from io import StringIO
from subprocess import Popen, PIPE


def run_mucg83(cmd, **composition):
    proc = Popen(cmd,
                 stdin=PIPE,
                 stdout=PIPE,
                 stderr=PIPE)

    comp = dict(C=0, Si=0, Mn=0, Ni=0, Mo=0, Cr=0, V=0, Co=0, Cu=0, Al=0, W=0)
    comp.update(composition)

    options = ('1\n'
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
               '0').format(**comp)

    # Passes compositions to mucg83 and reads output
    # Important information is stored in stdout
    stdout, stderr = proc.communicate(options.encode())

    try:
        proc.kill()
    except Exception:
        pass

    return stdout, stderr


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
        df['SHEAR_CTEMP'].where(cond=df['SHEAR_CTEMP']
                                <= Bs_n, other=np.nan, inplace=True)

    return df, Ws, Bs_g, Bs_n, Ms


def get_transformation_temperature_CCT(TTT_curve_inv, Tini, cooling_rate, dT=1.0, maxit=1000):
    """
    Uses Scheil's method to get the transformation temperature during 
    continuous cooling from a TTT curve
    """
    curr_T = None

    if cooling_rate > 0:
        dt = dT/cooling_rate
        curr_T = Tini - dT/2.
        nucleation_time = 0.

        it = 0
        while nucleation_time < 1:
            increment = 0.
            try:
                increment = dt/TTT_curve_inv(curr_T)
            except Exception:
                pass

            nucleation_time += increment
            curr_T = curr_T - dT

            it += 1

            if it > maxit:
                print(('Maximum number of iterations ({:}) '
                       'reached for phi = {:e} K/s').format(maxit, cooling_rate))
                curr_T = None
                break
    else:
        print('cooling_rate has to be strictly larger than 0')

    return curr_T


def convert_TTT_to_CCT(t, T, Tini, cooling_rates, dT=1.0):
    from scipy.interpolate import interp1d

    # Ascending cooling rates
    cooling_rates = np.sort(cooling_rates)
    # Inverse interpolation function of the transformation C curve
    # (temperature is mapped into time)
    TTT_curve_inv = interp1d(T, t)

    tlist = []
    Tlist = []

    for cooling_rate in cooling_rates:
        T = get_transformation_temperature_CCT(TTT_curve_inv, Tini, cooling_rate)
        if T:
            tlist.append((Tini - T)/cooling_rate)
            Tlist.append(T)
        else:
            break

    return np.array(tlist), np.array(Tlist)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Python interface of MUCG83 for calculating TTT and CCT diagrams')
    parser.add_argument('-C', '--C', type=float, default=0., help='Carbon wt.%% (0.001-2.0)')
    parser.add_argument('-Si', '--Si', type=float, default=0., help='Silicon wt.%% (0.0-2.5)')
    parser.add_argument('-Mn', '--Mn', type=float, default=0., help='Manganese wt.%% (0.0-3.5)')
    parser.add_argument('-Ni', '--Ni', type=float, default=0., help='Nickel wt.%% (0.0-3.5)')
    parser.add_argument('-Mo', '--Mo', type=float, default=0., help='Molybdenum wt.%% (0.0-1.5)')
    parser.add_argument('-Cr', '--Cr', type=float, default=0., help='Chromium wt.%% (0.0-3.5)')
    parser.add_argument('-V', '--V', type=float, default=0., help='Vanadium wt.%% (0.0-1.5)')
    parser.add_argument('-Co', '--Co', type=float, default=0., help='Cobalt wt.%% (0.0-4.0)')
    parser.add_argument('-Cu', '--Cu', type=float, default=0., help='Copper wt.%% (0.0-4.0)')
    parser.add_argument('-Al', '--Al', type=float, default=0., help='Aluminium wt.%% (0.0-2.0)')
    parser.add_argument('-W', '--W', type=float, default=0., help='Tungsten wt.%% (0.0-4.0)')
    parser.add_argument('-Tini', '--Tini', type=float, default=900.,
                        help='Initial continuous cooling temperature')
    parser.add_argument('-cmd', '--cmd',
                        default='bin/mucg83.exe' if platform.system() == 'Windows' else 'bin/mucg83',
                        help='Path to mucg83 executable')
    parser.add_argument('-e', '--exp', action='store_true', help='Export to .xlsx format')

    args = parser.parse_args()

    args = vars(args)
    cmd = args.pop('cmd')
    Tini = args.pop('Tini')
    export = args.pop('exp')

    composition = args

    stdout, stderr = run_mucg83(cmd, **composition)
    # Parse stdout into pandas DataFrame df and critical temperatures Ws, Bs_g, Bs_n, and Ms
    df, Ws, Bs_g, Bs_n, Ms = parse_stdout(stdout.decode())

    title = ['{}{}'.format(v, k) for k, v in composition.items() if v > 0]
    title.insert(0, 'Fe')
    title = '-'.join(title)

    if export:
        try:
            fout = '{}.xlsx'.format(title)
            print('Exporting data to {}'.format(fout))
            df.to_excel(fout)
        except Exception as ex:
            print(ex)

    df_no_nan = df.dropna()

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 6))

    # Plot TTT
    ax1.plot(df['DIFFT'], df['CTEMP'], label='Ferrite + Pearlite (TTT)')
    ax1.plot(df_no_nan['SHEART'], df_no_nan['SHEAR_CTEMP'], label='Bainite (TTT)')

    # Plot CCT
    T_min = Ms if Ms is not None else 25

    t_min = min(df['DIFFT'].min(), df['SHEART'].min())
    phi_min_log = np.log10(Tini/t_min)
    cooling_rates = 10**np.linspace(phi_min_log - 6, phi_min_log, 240)

    t, T = convert_TTT_to_CCT(df['DIFFT'], df['CTEMP'], Tini, cooling_rates)
    ax2.plot(t[T > T_min], T[T > T_min], label='Ferrite + Pearlite (CCT)')
    t, T = convert_TTT_to_CCT(df_no_nan['SHEART'], df_no_nan['SHEAR_CTEMP'], Tini, cooling_rates)
    ax2.plot(t[T > T_min], T[T > T_min], label='Bainite (CCT)')

    for cooling_rate in cooling_rates[::10]:
        T = np.linspace(Tini, 25, 100)
        t = (Tini - T)/cooling_rate
        ax2.plot(t, T, 'k:', lw=1)

    ax1.set_xscale('log')
    # ax1.set_xlim(1e-2, 1e6)
    ax1.set_ylim(100, Tini)
    ax1.set_xlabel('Time (s)')
    ax1.set_ylabel(u'Temperature (°C)')

    ax2.set_xscale('log')
    # ax2.set_xlim(1e-2, 1e6)
    ax2.set_ylim(100, Tini)
    ax2.set_xlabel('Time (s)')
    ax2.set_ylabel(u'Temperature (°C)')

    fig.suptitle(title)

    if Bs_n:
        print('Bainite start temperature:', Bs_n, 'oC')
        ax1.axhline(Bs_n, color='r', ls=':')
        ax1.text(ax1.get_xlim()[0]*1.5, Bs_n + 10, 'Bainite start', color='r')
    if Ms:
        print('Martensite start temperature:', Ms, 'oC')
        ax1.axhline(Ms, color='b', ls=':')
        ax1.text(ax1.get_xlim()[0]*1.5, Ms + 10, 'Martensite start', color='b')

    ax1.legend()
    ax2.legend()

    plt.show()
