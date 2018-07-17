from toolz.functoolz import compose

from example_lib import ultimate_answer


def main():

    def msg(ans):
        return "Answer to the Ultimate Question of Life,\n" \
            + "    the Universe, and Everything: %s" % ans

    compose(print, msg)(ultimate_answer())


if __name__ == '__main__':
    main()
