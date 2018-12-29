import pandas as pd

file = "~/dropbox/dmclass2018train.csv"

df_train = pd.read_csv(file)

print(df_train.info())
