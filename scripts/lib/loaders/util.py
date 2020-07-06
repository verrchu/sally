import yaml

def load_yaml(path):
    return yaml.load(open(path, 'r'), Loader=yaml.Loader)
