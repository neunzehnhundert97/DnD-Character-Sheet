import re
import requests
import subprocess


def replacer(offline: bool, type):
    def inner(match: re):
        url: str = match.group(1)

        if url.startswith("http"):
            if offline:
                req = requests.get(url)
                file = req.content.decode("utf-8")
            else:
                return match.group(0)
        else:
            with open(url) as f:
                file = f.read().replace("//# sourceMappingURL=dndsheetjs-opt.js.map", "")

        if type == "js":
            return "<script>\n" + file + "</script>"
        else:
            return "<style>\n" + file + "</style>"

    return inner


# Run sbt for the current version
print("Running sbt")
subprocess.run("sbt fullOptJS", shell=True)

# Load sheet
with open("Sheet.html") as file:
    sheet_raw = file.read()

sheet_raw = sheet_raw.replace("dndsheetjs-fastopt.js", "dndsheetjs-opt.js")

# Create completly inlined verion
sheet_offline = re.sub(r'<link.*href="(.*)".*>', replacer(True, "css"), sheet_raw)
sheet_offline = re.sub(r'<script.*src="(.*)".*></script>', replacer(True, "js"), sheet_offline)

# Create partially inlined version
sheet_online = re.sub(r'<link.*href="(.*)".*>', replacer(False, "css"), sheet_raw)
sheet_online = re.sub(r'<script.*src="(.*)".*></script>', replacer(False, "js"), sheet_online)

with open("../Sheet_offline.html", "w") as file:
    file.write(sheet_offline)

with open("../Sheet_online.html", "w") as file:
    file.write(sheet_online)
