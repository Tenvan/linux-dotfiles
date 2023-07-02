# filter_plugins/custom_filters.py

from ansible.errors import AnsibleFilterError


def transform_package(package, option):
    result = None

    if option == "-":
        result = ""
    elif option == "+":
        result = package
    else:
        result = option

    print("Package " + str(package) + " / " + str(option) + " => " + str(result))
    return result


def remap_package_node(values):
    result = []

    for value in values:
        print("Daten vorher:")
        print(value)

        package = value["all"]

        if not package.startswith("#"):
            print(package)
            new = {
                "name": package,
                "arch": transform_package(package, value["arch"]),
                "arco": transform_package(package, value["arco"]),
                "manjaro": transform_package(package, value["manjaro"]),
                "redhat": transform_package(package, value["redhat"]),
                "rhel": transform_package(package, value["rhel"]),
                "fedora": transform_package(package, value["fedora"]),
                "debian": transform_package(package, value["debian"]),
                "ubuntu": transform_package(package, value["ubuntu"]),
            }
            print("Daten nacher:")
            print(new)

            result.append(new)

    return result


class FilterModule(object):
    def filters(self):
        return {"remap_package_node": remap_package_node}
