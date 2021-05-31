from parser_vk import get_all_friends, get_all_members, get_all_users, get_all_posts
import pandas as pd
from tqdm import tqdm

user_ids = get_all_members('sawoc')
users_info = get_all_users(user_ids, fields=['sex', 'bdate', 'city', 'country', 'education', 'occupation'])
community_posts = get_all_posts('sawoc', 0)



info = pd.DataFrame({

    'Id': [user['id'] for user in users_info],
    'Label': [user['id'] for user in users_info],
    'first name': [user['first_name'] for user in users_info],
    'last name': [user['last_name'] for user in users_info],
    'sex': [user.get('sex') for user in users_info],
    'date of birth': [user.get('bdate') for user in users_info],
    'country': [user.get('country', {}).get('title') for user in users_info],
    'city': [user.get('city', {}).get('title') for user in users_info],
    'occupation': [user.get('occupation', {}).get('name') for user in users_info],
    'occupation type': [user.get('occupation', {}).get('type') for user in users_info],
    'education status': [user.get('education_status') for user in users_info],

})


#print(info.head())


"""""
friends = get_all_friends(users_info)

friendship_matrix = {}
for member1 in tqdm(user_ids, desc='Friendship Matrix'):
    for member2 in user_ids:
        if not friendship_matrix.get(member1):
            friendship_matrix[member1] = {}
        friendship_matrix[member1][member2] = int(member2 in friends[member1])

pd.DataFrame(friendship_matrix).to_csv('friendship_matrix.csv')

"""""

activity = {}

for user in user_ids:
    for post in community_posts:
        if not activity.get(user):
            activity[user] = 0
        if post.get('signer_id') == user:
            activity[user] += 1


info['activity'] = info['Id'].map(activity)
print(info.head())
info.to_csv('user_information.csv', encoding='utf-16')