from sklearn.ensemble.forest import RandomForestClassifier

def read(fname):
    labels, data = [],[]
    with open(fname) as f:
        for s in f:
            ss = s.split()
            labels.append(int(ss[-1]))
            data.append(map(float, ss[:-2]))
    return labels, data

trainset = read('./trainset')
testset  = read('./testset')

clf = RandomForestClassifier(n_estimators=10)
clf.fit(trainset[1], trainset[0])
print clf.predict(testset[1])
print testset[0]
