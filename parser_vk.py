import requests
from requests import get
from time import sleep
from tqdm import tqdm


def check_error(response, show=False):
    if response.get('error'):
        if show:
            print('Error {code} ({message})'.format(code=response['error']['error_code'],
                                                    message=response['error']['error_msg']))
        return True
    return False


def get_members(group_id, offset, sort='id_asc', fields=None) -> dict:
    from access import token, v, sleep_time
    params = {
        'group_id': group_id,
        'sort': sort,
        'offset': offset,

        'access_token': token,
        'v': v
    }
    if fields:
        params['fields'] = ','.join(fields)

    response = get('https://api.vk.com/method/groups.getMembers', params=params).json()
    sleep(sleep_time)
    if check_error(response):
        sleep(sleep_time)
        response = get('https://api.vk.com/method/groups.getMembers', params=params).json()
        if check_error(response, show=True):
            return {}

    return response['response']


def get_all_members(group_id, sort='id_asc', fields=None) -> list:
    response = get_members(group_id, 0, sort=sort, fields=fields)
    if not response:
        return []

    count = response['count']
    items = []
    for offset in tqdm(range(0, count + 1000, 1000), desc='Get Group Members'):
        response = get_members(group_id, min(offset, count), sort=sort, fields=fields)
        if not response:
            break
        items = [*items, *response['items']]

    return items


def get_users(user_ids: list, fields=None) -> list:
    from access import token, v, sleep_time
    params = {
        'user_ids': ','.join(map(str, user_ids)),

        'access_token': token,
        'v': v
    }
    if fields:
        params['fields'] = ','.join(fields)

    response = get('https://api.vk.com/method/users.get', params=params).json()
    sleep(sleep_time)
    if check_error(response):
        sleep(sleep_time)
        response = get('https://api.vk.com/method/users.get', params=params).json()
        if check_error(response, show=True):
            return []

    return response['response']


def get_all_users(user_ids: list, fields=None) -> list:
    users_info = []
    for count in tqdm(range(0, len(user_ids) + 1000, 1000), desc='User Information'):
        user = get_users(user_ids[count:min(count + 1001, len(user_ids))], fields=fields)
        users_info.extend(user)
    return users_info


def get_friends(user_id, fields=None) -> list:
    from access import token, v, sleep_time
    params = {
        'user_id': user_id,

        'access_token': token,
        'v': v
    }
    if fields:
        params['fields'] = ','.join(fields)

    response = get('https://api.vk.com/method/friends.get', params=params).json()
    sleep(sleep_time)
    if check_error(response):
        sleep(sleep_time + 3)
        response = get('https://api.vk.com/method/friends.get', params=params).json()
        if check_error(response, show=True):
            return []

    return response['response']['items']


def get_all_friends(users_info: list, fields=None) -> dict:
    all_friends = {}
    for user in tqdm(users_info, desc='Get All Friends'):
        if user.get('is_closed') or user.get('deactivated'):
            all_friends[user['id']] = set()
            continue
        try:
            all_friends[user['id']] = set(get_friends(user['id']))
        except requests.exceptions.ConnectionError:
            sleep(3)
            print('Connection Lost. Retry...')
            try:
                all_friends[user['id']] = set(get_friends(user['id']))
            except requests.exceptions.ConnectionError:
                print('Connection Lost and Jopa')
                all_friends[user['id']] = set()

    return all_friends


def get_community_posts(domain, offset, count=100, filter='all'):
    from access import token, v, sleep_time
    params = {
        'domain': domain,
        'offset': offset,
        'count': count,
        'filter': filter,

        'access_token': token,
        'v': v
    }
    response = get('https://api.vk.com/method/wall.get', params=params).json()
    sleep(sleep_time)
    if check_error(response):
        sleep(sleep_time + 3)
        response = get('https://api.vk.com/method/wall.get', params=params).json()
        if check_error(response, show=True):
            return []

    return response['response']


def get_all_posts(domain, offset, count=100, filter='all'):
    response = get_community_posts(domain, offset=0, count=count)
    if not response:
        return []

    count = response['count']
    items = []
    for offset in tqdm(range(0, count + 100, 100), desc='Get All Posts'):
        response = get_community_posts(domain, offset=min(offset, count), count=count)
        if not response:
            break
        items = [*items, *response['items']]

    return items
