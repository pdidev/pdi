from deisa import Deisa

# Scheduler file name and configuration file
scheduler_info = '/tmp/scheduler.json'

# Initialize Deisa
Deisa = Deisa(scheduler_info, nb_workers=1)

# Get client
client = Deisa.get_client()

# either: Get data descriptor as a list of Deisa arrays object
arrays = Deisa.get_deisa_arrays()
# Select data
gt = arrays["global_t"][...]

# Check contract
arrays.check_contract()

##########################
# Perform analysis here
##########################

arrays.validate_contract()
# del gt

client.shutdown()
