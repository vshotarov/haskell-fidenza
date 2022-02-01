#!/bin/python
import random
import os
import sys
import shutil
import math

params = {
        "collisionCheck" : [("noOverlap",.8),("relaxed",.15),("anythingGoes",.05)],
        "colours"        : ["luxe","rad","golfSocks","baked",
                           "politique","cool","am","whiteOnCream",
                           "blackAndWhite"],
        "haveMargin"     : ["yes","no"],
        "outlined"       : ["yes","no"],
        "scale"          : [("small",.05),("medium",.1),
                            ("large",.2),("jumbo",.3),("jumboXL",.1),
                            ("microUniform",.05),("uniform",.2)],
        "shapeAngles"    : [("curved",.8),("sharp",.2)],
        "softShapes"     : [("yes",.05),("no",.95)],
        "superBlocks"    : [("yes",.2),("no",.8)],
        "turbulence"     : [("none",.1),("low",.3),("mid",.5),("high",.1)]
}

def to_distribution(values):
    if isinstance(values[0], str):
        p = 1/len(values)
        return [(v,p) for v in values]
    probabilities_sum = sum([p for v,p in values])
    return sorted([(v,p/probabilities_sum) for v,p in values],
                  key=lambda x:x[1])

def sample_distribution(distribution, x):
    for v,p in distribution:
        if x <= p:
            return v
        x -= p
    # Should never hit this, but still..
    return distribution[-1][0]

def seed_to_params(seed):
    generator = random.Random()
    generator.seed(seed)

    distributions = {k: to_distribution(v) for k,v in params.items()}

    seeded_params = {k: sample_distribution(v,generator.random())
                     for k,v in distributions.items()}

    # Add a 10% probability for creating a colour schemed derived
    # randomly from an existing one
    if generator.random() <= 0.1:
        seeded_params["colours"] = "derived"\
                                 + seeded_params["colours"].title()[0]\
                                 + seeded_params["colours"][1:]

    return seeded_params

if __name__ == "__main__":
    if len(sys.argv) < 2:
        seed = math.floor(random.random() * 10e15) # get a random seed
        print("Using global seed", seed)
        random.seed(seed)

        if not os.path.exists("img/random"):
            print("img/random directory missing. Creating..")
            os.makedirs("img/random")

        for i in range(100):
            iter_seed = math.floor(random.random() * 10e15)
            seeded_params = seed_to_params(iter_seed)

            cmd = ("cabal run Fidenza -O2 -- --seed %i " % iter_seed) +\
                      " ".join(["--%s %s" % (k,v) for k,v in seeded_params.items()])
            os.system(cmd)

            if os.path.exists("img/random/%i.png" % iter_seed):
                with open("LOG","a") as f:
                    f.write("CLASH on %i" % iter_seed)

            shutil.move("Fidenza.png","img/random/%i.png" % iter_seed)
    else:
        iter_seed = int(sys.argv[1])
        seeded_params = seed_to_params(iter_seed)

        cmd = ("cabal run Fidenza -O2 -- --seed %i " % iter_seed) +\
                  " ".join(["--%s %s" % (k,v) for k,v in seeded_params.items()])
        os.system(cmd)
