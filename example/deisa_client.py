from deisa import Deisa

# Scheduler file name and configuration file
scheduler_info = '/tmp/scheduler.json'

# Initialize Deisa
deisa = Deisa(scheduler_info, nb_workers=1, use_ucx=False)

# either: Get data descriptor as a list of Deisa arrays object
arrays = deisa.get_deisa_arrays()

# Select data
gt = arrays["global_t"][...]

# Check contract
arrays.check_contract()


##########################
# Perform analysis here
##########################


arrays.validate_contract()

deisa.wait_for_last_bridge_and_shutdown()
