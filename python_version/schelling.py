import matplotlib.pyplot as plt
import itertools
import random
import copy


class Schelling:
    def __init__(self, width=100, height=100, empty_ratio=0.1, tolerence=0.5,
                 rounds=100, races=2):
        """Initialize model."""
        self.width = width
        self.height = height
        self.races = races
        self.empty_ratio = empty_ratio
        self.tolerence = tolerence
        self.rounds = rounds
        self.board = self.create_board()

    def create_board(self):
        """Fill board with agents."""
        board = list(itertools.product(range(self.width), range(self.height)))
        return(board)
