#!/usr/bin/env python

import argparse


# We've got a canvas with pixels going this way:
#     W A R P
#
#    ^        A
#    |        B
#    |        B
# x -+--------->
#   0|
#    y
#
#
# Meanwhile we store data in memory like this:
#
# this.rows:
#
#  3: [X, X, X, X, X]
#  2: [X, X, X, X, X]
#  1: [X, X, X, X, X]
#  0: [X, X, X, X, X]

class Cell(object):
    def __init__(self, color):
        self.color = int(color)
        self.z_index = 0

class Canvas(object):
    def __init__(self, input_path):
        self.width = 0
        self.height = 0
        self.rows = []
        self.palette = set()
        self.warp = []
        self.abb = []
        with open(input_path, 'r') as handle:
            lines = handle.readlines()
            self.height = lines
            row_number = 0
            for row in lines:
                if len(row) == 0:
                    raise RuntimeError(
                        'Data file should not contain empty strings. '
                        'Row {}'.format(row_number)
                elements = row.split(',')
                if self.width == 0:
                    self.width = len(elements)
                elif len(elements) != self.width:
                    raise RuntimeError(
                        'Data file is incosistent. '
                        'Rows always should have same number of elements')
                self.rows.append([Cell(el) for el in elements])

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
        return self.rows[y][x].color

    def get_palette(self):
        '''
        Get set of colors used in canvas.
        '''
        if not self.palette:
            for row in self.rows:
                for element in row:
                    if element not in self.palette:
                        self.palette.add(element)
        return self.palette


    def try_fill_abb_and_warp(self, ):
        abb = [None for e in xrange(self.height)]
        warp = [None for e in xrange(self.width)]
        for x in 
        if warp[x] is None:
            warp[x] = 


def _parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('data_file', help='File with input data')
    return parser.parse_args()

if __name__ == '__main__':
    args = _parse_args()
    canvas = Canvas(args.data_file)
    print 'Palette is: {}'.format(canvas.get_palette())
