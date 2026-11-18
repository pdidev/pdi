# Python script run by the Damaris server cores for test_damaris_pyscript.yml.
# For each iteration it runs on, it writes `pyscript_It<iteration>.txt` in the working directory,
# containing the number of `values` blocks received and the sum of all of them.

def main(DD):
    iter_dict = DD['iteration_data']
    iteration = iter_dict['iteration']
    blocks = iter_dict['values']['numpy_data']
    total = sum(int(block.sum()) for block in blocks.values())
    with open('pyscript_It' + str(iteration) + '.txt', 'w') as result:
        result.write(str(len(blocks)) + ' ' + str(total) + '\n')


if __name__ == '__main__':
    main(DamarisData)
