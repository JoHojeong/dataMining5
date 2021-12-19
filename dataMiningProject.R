# 1. 데이터를 획득하고 모델을 적용하기 위한 준비과정
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')

ucla$admit = factor(ucla$admit)
str(ucla)

# 2. 학습데이터와 테스트데이터 분리과정
# 데이터를 6:4 비율(6은 학습데이터, 4는 테스트데이터(예측)로 사용)
n = nrow(ucla)
i = 1:n
train_list = sample(i, n*0.6) # 전체 데이터 중 60%를 랜덤 샘플링(학습 데이터)
test_list = setdiff(i, train_list) # 학습데이터를 제외한 나머지 데이터(테스트 데이터)
ucla_train = ucla[train_list, ]
ucla_test = ucla[test_list, ]

# 데이터 및 개수 확인
ucla_train # 데이터
nrow(ucla_train) # 개수

ucla_test # 데이터
nrow(ucla_test) # 개수

