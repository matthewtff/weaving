#!/usr/bin/env python

import argparse
import copy

class ZIndex(object):
    LOWER = -1 # Use color of warp thread
    UPPER = 1 # Use color of weft thread
    NONE = 0


class RunCheckType(object):
    ANY = 0
    WARP = 1


def _check_at_a_run(cells_list, max_at_a_run, check_type=RunCheckType.ANY):
    if max_at_a_run == -1:
        return True
    index_at_a_run = 0
    last_index = None
    for cell in cells_list:
        assert cell.color is not None
        assert cell.z_index is not ZIndex.NONE
        if check_type == RunCheckType.ANY:
            should_check = True
        else:
            assert check_type == RunCheckType.WARP
            should_check = cell.z_index == ZIndex.LOWER
        if last_index == cell.z_index and should_check:
            index_at_a_run += 1
            if index_at_a_run > max_at_a_run:
                return False
        else:
            index_at_a_run = 0
            last_index = cell.z_index
    return True


def _check_columns(rows, max_at_a_run):
    number_of_rows = len(rows)
    if number_of_rows == 0:
        return True
    for index in xrange(0, number_of_rows - 1):
        current_row = rows[index]
        next_row = rows[index + 1]
        if current_row.cells[0].z_index == next_row.cells[0].z_index:
            return False
        if current_row.cells[-1].z_index == next_row.cells[-1].z_index:
            return False
    for index in xrange(0, rows[0].length):
        column = [row.cells[index] for row in rows]
        if not _check_at_a_run(column, max_at_a_run,
                               check_type=RunCheckType.WARP):
            return False
    return True


def _fill_row(row, warp, shift):
    """
    Recursive. Returns list of succeeded (row, warp) pairs.
    """

    if shift == row.length:
        return [(row, tuple(warp))] if row.check() else []

    results = []
    assert row.cells[shift].z_index == ZIndex.NONE
    current_color = row.cells[shift].color
    assert current_color is not None

    if current_color == row.weft_color or row.weft_color is None:
        weft_row = copy.deepcopy(row)
        weft_row.cells[shift].z_index = ZIndex.UPPER
        weft_row.weft_color = current_color
        results += _fill_row(weft_row, warp, shift + 1)

    if current_color == warp[shift] or warp[shift] is None:
        warped_row = copy.deepcopy(row)
        warped_row.cells[shift].z_index = ZIndex.LOWER
        new_warp = warp[:] if warp[shift] is None else warp
        new_warp[shift] = current_color
        results += _fill_row(warped_row, new_warp, shift + 1)

    return results

def _fill_warp(canvas, max_at_a_run, warp, rows):
    """
    Recursive. Returns list of succeeded rows
    """
    row_index = len(rows)

    if not _check_columns(rows, max_at_a_run):
        return []

    if row_index == canvas.height:
        return [(rows, tuple(warp))]

    results = []
    got_rows = set(_fill_row(canvas.rows[row_index], warp, 0))
    for row, next_warp in got_rows:
        results += _fill_warp(
            canvas, max_at_a_run, list(next_warp), rows[:] + [row])

    return results


class Cell(object):
    """
    We've got a canvas with pixels going this way:
        W A R P

       ^        W
       |        E
       |        F
       |        T
    x -+--------->
      0|
       y


    Meanwhile we store data in memory like this:

    this.rows:

     3: [X, X, X, X, X]
     2: [X, X, X, X, X]
     1: [X, X, X, X, X]
     0: [X, X, X, X, X]
    """

    def __init__(self, color):
        self.color = int(color)
        self.z_index = ZIndex.NONE


    def __str__(self):
        index_char = '*'
        if self.z_index == ZIndex.UPPER:
            index_char = '^'
        elif self.z_index == ZIndex.LOWER:
            index_char = 'v'
        return '{}:{}'.format(self.color, index_char)

    def __repr__(self):
        return self.__str__()

    def __eq__(self, other):
        return self.color == other.color and self.z_index == other.z_index


class Row(object):
    def __init__(self, max_at_a_run, cells):
        self.max_at_a_run = max_at_a_run
        self.weft_color = None

        assert type(cells) is list
        self.cells = cells

    @property
    def length(self):
        return len(self.cells)

    def check(self):
        """
        Returns True if row passes |max_at_a_run| check. False otherwise.
        """
        return _check_at_a_run(self.cells, self.max_at_a_run)

    def __str__(self):
        contents = ', '.join([repr(cell) for cell in self.cells])
        return '[ {} : | {} | ]'.format(self.weft_color, contents)

    def __repr__(self):
        return self.__str__()


class Canvas(object):
    def __init__(self, input_path, max_at_a_run):
        self.width = 0
        self.height = 0
        self.rows = []
        self.palette = set()
        self.warp = []
        with open(input_path, 'r') as handle:
            lines = handle.readlines()
            self.height = len(lines)
            row_number = 0
            for row in lines:
                if len(row) == 0:
                    raise RuntimeError(
                        'Data file should not contain empty strings. '
                        'Row {}'.format(row_number))
                elements = row.split(',')
                if self.width == 0:
                    self.width = len(elements)
                elif len(elements) != self.width:
                    raise RuntimeError(
                        'Data file is incosistent. '
                        'Rows always should have same number of elements')
                self.rows.append(
                    Row(max_at_a_run, [Cell(el) for el in elements]))

                ++row_number
        # Since rows in file go from higher to lower and we always appended
        # them to the end of list, rows should be reversed.
        self.rows.reverse()

    def get_color(self, x, y):
        '''
        To access element at [x, y] we first select a row in self.rows by y,
        then take x'th element in that row. Assume |x| and |y| start from 0.
        '''
        assert x < self.width
        assert y < self.height
        row = self.rows[y]
        return row.cells[x].color

    def get_palette(self):
        '''
        Get set of colors used in canvas.
        '''
        if not self.palette:
            for row in self.rows:
                for cell in row.cells:
                    if cell.color not in self.palette:
                        self.palette.add(cell.color)
        return self.palette


    def try_fill_weft_and_warp(self, max_at_a_run):
        warp = [None for e in xrange(self.width)]
        return _fill_warp(canvas, max_at_a_run, warp, [])

    def __str__(self):
        return '\n'.join([repr(row) for row in self.rows])

    def __repr__(self):
        return self.__str__()


def _parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('data_file', help='File with input data')
    parser.add_argument('--max-warp-at-a-run',
                        help='Maximum number of threads in a column',
                        default=-1, type=int)
    parser.add_argument('--max-weft-at-a-run',
                        help='Maximum number of threads in a row',
                        default=-1, type=int)
    return parser.parse_args()


if __name__ == '__main__':
    args = _parse_args()
    canvas = Canvas(args.data_file, args.max_weft_at_a_run)
    print 'Canvas:'
    print canvas
    print 'Palette is: {}'.format(canvas.get_palette())
    results = canvas.try_fill_weft_and_warp(args.max_warp_at_a_run)
    for rows, warp in results:
        print ''
        print 'Solution:'
        print 'Warp: {}'.format(warp)
        for row in rows:
            print row
    print 'Number of solutions: {}'.format(len(results))
