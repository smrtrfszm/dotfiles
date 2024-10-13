import os
import atexit
import readline

if 'XDG_STATE_HOME' in os.environ:
    history = os.path.join(os.path.expanduser(os.environ['XDG_STATE_HOME']), 'python', 'history')
else:
    history = os.path.join(os.path.expanduser('~'), '.local', 'state', 'python', 'history')

history = os.path.abspath(history)
history_dir, _ = os.path.split(history)
os.makedirs(history_dir, exist_ok=True)

try:
    readline.read_history_file(history)
    readline.set_history_length(1000)
except FileNotFoundError:
    pass

atexit.register(readline.write_history_file, history)
