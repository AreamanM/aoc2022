class Dir:

    def __init__(self, files, children, parent):
        self.files = files
        self.children = children
        self.parent = parent

    def __repr__(self) -> str:
        return f"Dir({self.files=}, {self.children=}, {self.parent=})"


def get_input():
    with open("day7.input") as f:
        return list(map(lambda x: x[:-1], f.readlines()))


def make_root(sh_hist: list[str]) -> tuple[Dir, list[str]]:
    contents, rest = span(
                lambda x: is_dir(x) or is_file(x),
                dropwhile(is_cmd, sh_hist))

    root = fill_dir(contents, Dir([], {}, None))

    return (root, rest)


# we can assume everything in here is a file or a dir
def fill_dir(contents: list[str], dir: Dir):
    for listing in contents:
        if is_dir(listing):
            dir.children[listing.split()[-1]] = Dir([], {}, dir)
        else:
            dir.files.append(int(listing.split()[0]))
    return dir


def traverse(sh_hist: list[str], root: Dir):
    curdir = root
    idx = 0
    while idx < len(sh_hist):
        line = sh_hist[idx]
        # since we skip over file and dir listings, it has to be a cmd
        if line.split()[1] == "cd":
            if line.split()[2] == "..":
                curdir = curdir.parent
            else:
                curdir = curdir.children[line.split()[2]]
            idx += 1
        elif line.split()[1] == "ls":
            contents = takewhile(
                lambda x: not is_cmd(x), sh_hist[idx + 1:])
            idx += len(contents) + 1
            curdir = fill_dir(contents, curdir)
    return root
    

def is_file(line: str) -> bool:
    return not (is_cmd(line) or is_dir(line))


def is_cmd(line: str) -> bool:
    return line[0] == "$"


def is_dir(line: str) -> bool:
    return line[:3] == "dir"


def takewhile(p, xs):
    for i, x in enumerate(xs):
        if not p(x):
            return xs[:i]
    return xs


def dropwhile(p, xs):
    for i, x in enumerate(xs):
        if not p(x):
            return xs[i:]
    return xs


def span(p, xs):
    for i, x in enumerate(xs):
        if not p(x):
            return (xs[:i], xs[i:])
    return ([], xs)

root, rest = make_root(get_input())
# bottom up tree of all the files
filesys = traverse(rest, root)


def file_sizes(dir):
    sizes = []

    def traverse_fs(root):
        nonlocal sizes

        if not root:
            return

        sizes += root.files

        for child in root.children.values():
            sizes += [file_sizes(child)]


    traverse_fs(dir)

    return sizes


small_dirs = [] 

def dir_size_part1(dir):
    global small_dirs

    size = 0

    for listing in dir:
        # it's a directory instead
        if isinstance(listing, list):
            subdir_size = dir_size_part1(listing)
            if subdir_size <= 100000:
                small_dirs.append(subdir_size)
            size += subdir_size
        else:
            size += listing

    return size


MAX_SPACE = 70000000
MIN_NEEDED = 30000000 

sizes = file_sizes(filesys)
curr_space_taken = dir_size_part1(sizes)
print(f"part 1: {sum(small_dirs)}")
to_remove = MIN_NEEDED - (MAX_SPACE - curr_space_taken)

def dir_size_part2(dir):
    global small_dirs, to_remove

    size = 0

    for listing in dir:
        # it's a directory instead
        if isinstance(listing, list):
            subdir_size = dir_size_part2(listing)
            if subdir_size >= to_remove:
                small_dirs.append(subdir_size)
            size += subdir_size
        else:
            size += listing

    return size

small_dirs.clear() # clear this for part 2's answer
dir_size_part2(sizes)
print(f"part 2: {min(small_dirs)}")
