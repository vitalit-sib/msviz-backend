# msViz Backend
A mass spectrometry visualization platform, developed by the PAF & Vital-IT, Swiss Institute of Bioinformatics.

## Development

You must have Java installed (tested with 7)

 1. `git clone` the project locally
 2. `cd msviz-backend`
 3. `./activator ui`

Then, you can run the test. Edit the source code with your preferred editor (from vi to Intellij). Test will be ran in the window at each save.

### Run
We have a running play application, so in development mode, it can be launched interactively (reloaded each time a source file is saved)

    ./activator ~run

## Data storage
The backend store in, for the time being, only a mongo server. Download and install mongodb. Launch the daemon with a command like

    mongod --fork --dbpath $HOME/work/mongodb/db --logpath $HOME/work/mongodb/mongod.log

To check that everything is running ok, you can then connect with a mongo-shell via

    mongo ms_viz

The configuration on where to hit the mongodb server is described in the conf/application.conf file.


## Authors
Roman Mylonas (roman.mylonas@isb-sib.ch), Trinidad Martin (trinidad.martin@isb-ib.ch) and  Alexandre Masselot (alexandre.masselot@isb-sib.ch)
