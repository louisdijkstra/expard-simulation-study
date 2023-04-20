# Examples of running jobs with batchtools on the BIPS cluster

## Included files:
* `run.R`: Runs the main experiment
* `bips-cluster-crash-course.pdf`: A few slides on how to work with the BIPS cluster
* `config.R` and `torque.tmpl`: Config for the BIPS cluster -- don't change. See below.

## Setup

You'll need some R packages: 

```r
install.packages(c("CVN", "CVNSim", "tidyverse", "hmeasure", "batchtools", "ggplot2"))
source("install-packages-github.R")
```

## Notes on cluster-specific settings

- In batchtools' `submitJobs()` function:
  - Ensure you're using `chunks.as.arrayjobs = TRUE`
  - `walltime` is set to 10 days - higher is forbidden, lower is not helpful
  - `chunk.size = 50` groups the jobs in chunks of 50, meaning 50 jobs at a time will be submitted. 
  - `max.concurrent.jobs = 40` submit the next chunk of jobs if there's less than 40 running. 
    - This can be increased if the cluster is currently not used by many others.
    - Avoid submitting thousands of jobs - the cluster *will* crash.


# General cluster usage

The cluster has a login node with internet access (for cloning git repositories and installing R packages), and compute nodes for doing the hard work.  
Don't do any hard work on the login node, either use batchtools to distribute jobs on the compute nodes or log into a compute node to work there interactively if necessary.

## Login

- Retrieve your access information (cluster IP, username, password) from IT
- To avoid having to enter your password, you can set up an SSH keypair.
  - [Instructions for VSCode](https://code.visualstudio.com/docs/remote/ssh-tutorial#_set-up-ssh)

- Use `qsub -I` to log into a compute node if required.


## First time cluster setup

- To use R, you have to load the R module on the cluster:
  - List available R versions: `module avail R`
  - Before loading an R module, you need `mpi`: `module load mpi/openmpi/1.8.5`
- Automatically load modules on login by adding the following to your `~/.bashrc`:

```sh
module load mpi/openmpi/1.8.5
module load R/4.0.2
module list
```

- Copy the config files `config.R` and `torque.tmpl` from above to your configuration directory:

```sh
# create directory if needed
mkdir -p ~/.config/batchtools/
cp  config.R ~/.config/batchtools/
cp  torque.tmpl ~/.config/batchtools/
```

## Screen

It is recommended to [use `screen`](https://linuxize.com/post/how-to-use-linux-screen/) in the terminal for keeping your session open even if you log out of the cluster:

- `screen -S someproject`: Opens a screen session with name `someproject`
- Keyboard shortcuts for screen always start with `ctrl + A`, then
  - `C` creates a new screen window
  - `N` switches to the next screen window if available and cycles through them
  - `D` detaches the screen session with all windows
- `screen -ls`: Lists your screen sessions
- `screen -r <session-name>`: Attaches a screen session with name `<session-name>`

To have `screen` show you a little more information (e.g. open windows) in the bottom status bar, you can create a config file and add the following:

```sh
touch ~/.screenrc
echo "hardstatus on" >> ~/.screenrc
echo "hardstatus alwayslastline" >> ~/.screenrc
echo "hardstatus string '%{gk}[%{G}%H%{g}][%= %{wk}%?%-Lw%?%{=b kR}(%{W}%n*%f %t%?(%u)%?%{=b kR})%{= kw}%?%+Lw%?%?%= %{g}]%{=b C}%{W}'" >> ~/.screenrc
```

Note that like every other job on the cluster, screen sessions will also be killed after 10 days.

## Monitoring

- Useful commands to monitor jobs:
  - `qstat`: List all running jobs (of all users)
    - Jobs with `[]` indicate an array job, e.g. Job ID `329743[].bipscluster`
  - `qstat -u`: List all running jobs of the current user
  - `qstat -n`: List all running jobs of the current node
  - `qstat -r`: List all running jobs of the current user and node
  - See `man qstat`
- If you have to kill a job, use `qdel <job ID>` 


## VSCode

For convenience, it may be easiest to use the cross-platform [Visual Studio Code](https://code.visualstudio.com/) to access the cluster, as it integrates a terminal with SSH capabilities in a reasonably fancy editor.  
VSCode can be installed as a user on Windows without the need for administrative access.

Install the "Remote - SSH" extension and set up the cluster as a remote, optionally setting up SSH key authentication along the way ([see the docs here](https://code.visualstudio.com/docs/remote/remote-overview)).  
More detailed setup is possible but it's probably easiest to follow the [instructions provided by the extension](https://github.com/REditorSupport/vscode-R).

There's also an [extension for R](https://marketplace.visualstudio.com/items?itemName=Ikuyadeu.r), which allows you to more easily run code interactively.  
Produced output (e.g. plot PDF files) can be downloaded by right-clicking them in the file explorer pane in VS Code.

# More info, citation, etc.

- https://github.com/bips-hb/batchtools_example
- https://mllg.github.io/batchtools/index.html
