import pandas as pd
from tqdm import tqdm
import datetime


def check_columns(columns_list):
    assert columns_list[0] == 'eid', 'failure'
    columns_list = columns_list[1:]
    print(f'the columns number: {len(columns_list)}')
    set_var = set()
    for var in columns_list:
        set_var.add(var.split('-')[0])
    print(f'var_num = {len(set_var)}')
    return columns_list, set_var


class FY:
    def __init__(self, csv_file):
        self.csv_file = csv_file
        self.read()

    def read(self):
        self.df = pd.read_csv(self.csv_file, low_memory=False)  
        self.columns_list, self.set_var = check_columns(list(self.df.columns.values))
        self.columns_list, self.var_list = sorted(self.columns_list), sorted(list(self.set_var)) 
        print(self.columns_list)
        self.nrows = len(self.df)
        print(f'the num of rows: {self.nrows}')

    def cal_control_group(self, split=20000):
        sub_col1, sub_col2 = self.var_list[:-3].copy(), self.var_list[:-3].copy()
        for i, col in enumerate(sub_col1):
            sub_col1[i] = sub_col1[i] + '_1'
            sub_col2[i] = sub_col2[i] + '_2'
        new_col = ['eid'] + sub_col1 + sub_col2 + ['Endpoint1'] 
        survey_col = ['105010-0.0', '105010-1.0', '105010-2.0', '105010-3.0', '105010-4.0']  
        times = self.nrows // split + 1  
        for t in range(times):
            output = pd.DataFrame(columns=new_col)
            loop_start = t * split  
            loop_end = (t+1) * split  
            if loop_end > self.nrows:  
                loop_end = self.nrows
            for person_id in tqdm(range(loop_start, loop_end)):    
                Endpoint1 = self.df.loc[person_id, 'Endpoint1'] 
                record_id = 0
                should_record = True
                for i, col in enumerate(survey_col):
                    time_servey = self.df.loc[person_id, col]
                    if type(time_servey) != float:
                        record_id = i
                        break
                if Endpoint1 != 0:  
                    time_cancer = self.df.loc[person_id, f'40005-{Endpoint1 - 1}.0'] 
                    time_cancer = datetime.datetime.strptime(time_cancer, "%Y/%m/%d")  
                    time_record = self.df.loc[person_id, f'105010-{record_id}.0']
                    time_record = datetime.datetime.strptime(time_record, "%Y/%m/%d")
                    cmp_list = []  
                    for i, col in enumerate(survey_col):  
                        time_servey = self.df.loc[person_id, col]
                        if type(time_servey) == float: 
                            continue
                        time_slot = datetime.datetime.strptime(time_servey, "%Y/%m/%d")  
                        if time_slot < time_cancer:
                            cmp_list += [i]  
                        if time_record > time_cancer:
                            should_record = False
                    for var in self.var_list: 
                        if var in ['Endpoint1', '40005', '105010']:  
                            continue
                        search_list = []
                        for cmp in cmp_list:
                            search_list += [var + f'-{cmp}.0']  
                        mean = self.df.loc[person_id, search_list].mean()  #
                        output.loc[person_id, var+'_1'] = mean  
                else:
                  
                    for var in self.var_list:
                        if var in ['Endpoint1', '40005', '105010']:
                            continue
                        search_list = [var + '-0.0', var + '-1.0', var + '-2.0', var + '-3.0', var + '-4.0']
                        mean = self.df.loc[person_id, search_list].mean()
                        output.loc[person_id, var+'_1'] = mean
                if should_record:
                    for var in self.var_list:
                        if var in ['Endpoint1', '40005', '105010']:
                            continue
                        output.loc[person_id, var + '_2'] = self.df.loc[person_id, var + f'-{record_id}.0']
                output.loc[person_id, 'eid'] = self.df.loc[person_id, 'eid']
                output.loc[person_id, 'Endpoint1'] = self.df.loc[person_id, 'Endpoint1']
            output.to_csv(f'E:/deskbook/data_subset/pan_cancer1_{t}.csv')


if __name__ == '__main__':
    stat_obj = FY('E:/deskbook/data_subset/data_subset_1.csv')
    stat_obj.cal_control_group()