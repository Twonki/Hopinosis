import subprocess

def test_run_simple_hopinosis():
    subprocess.run(["Hopinosis","./Files/darkwing.txt", "2","0.51","0.51"],check=True)
    #assert r.status_code is 200

if __name__ == "__main__":
    test_run_simple_hopinosis()