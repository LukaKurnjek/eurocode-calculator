
from flask import Flask, jsonify

app = Flask(__name__)

def get_function(key):
    return_val = {"error": "No value."}
    file = open('keys3.txt', "r")
    for line in file:
        if key in line:
            return_val = line
    file.close()
    return return_val

def delete_function(key):
    return_val = {"status": "Deleted."}
    with open("keys3.txt", "r") as f:
        lines = f.readlines()
    with open("keys3.txt", "w") as f:
        for line in lines:
            if key not in line:
                f.write(line)

    with open("deleted.txt", "r") as f:
        file_content = f.read()
    
    with open("deleted.txt", "w") as f:
        file_content += key + "\n"
        f.write(file_content)

    return return_val

@app.route("/keys/KKK", methods=["GET"])
def get_countries0():
    return_val = get_function("KKK")
    return jsonify(return_val)

@app.route("/keys/KKK", methods=["DELETE"])
def delete_countries0():
    return_val = delete_function("KKK")
    return jsonify(return_val)

@app.route("/keys/NXJQMDEBHCP2S70GA65O", methods=["GET"])
def get_countries1():
    return_val = get_function("NXJQMDEBHCP2S70GA65O")
    return jsonify(return_val)

@app.route("/keys/NXJQMDEBHCP2S70GA65O", methods=["DELETE"])
def delete_countries1():
    return_val = delete_function("NXJQMDEBHCP2S70GA65O")
    return jsonify(return_val)

@app.route('/keys/HF75XLTWMZVR0DQAGP3K', methods=['GET'])
def get_countries2():
	return_val = get_function('HF75XLTWMZVR0DQAGP3K')
	return jsonify(return_val)

@app.route('/keys/HF75XLTWMZVR0DQAGP3K', methods=['DELETE'])
def delete_countries2():
	return_val = delete_function('HF75XLTWMZVR0DQAGP3K')
	return jsonify(return_val)

@app.route('/keys/GHZ31FMLIQ8C6S0V7UPR', methods=['GET'])
def get_countries3():
	return_val = get_function('GHZ31FMLIQ8C6S0V7UPR')
	return jsonify(return_val)

@app.route('/keys/GHZ31FMLIQ8C6S0V7UPR', methods=['DELETE'])
def delete_countries3():
	return_val = delete_function('GHZ31FMLIQ8C6S0V7UPR')
	return jsonify(return_val)
