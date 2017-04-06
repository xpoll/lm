package io.terminus.galaxy.user.service;

import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.galaxy.user.model.Notice;
import io.terminus.pampas.client.Export;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
public interface NoticeReadService {

    /**
     * 首页展示: 查询平台新闻/公告分页
     *
     * @param type 1:新闻 2:公告
     * @param pageNo 页码
     * @param pageSize 大小
     * @param search 搜索词
     * @param startAt 更新时间: 左区间
     * @param endAt 更新时间: 右区间
     * @return 新闻/公告分页
     */
    @Export(paramNames = {"type","pageNo","pageSize","search","startAt","endAt"})
    Response<Paging<Notice>> findNoticePlatform(Integer type, Integer pageNo, Integer pageSize, String search, String startAt, String endAt);

    /**
     * 店铺首页展示: 查询店铺新闻/公告分页
     * @param shopId 店铺ID
     * @param type 1:新闻 2:公告
     * @param pageNo 页码
     * @param pageSize 大小
     * @param search 搜索词
     * @param startAt 更新时间: 左区间
     * @param endAt 更新时间: 右区间
     * @return 新闻/公告分页
     */
    @Export(paramNames = {"shopId","type","pageNo","pageSize","search","startAt","endAt"})
    Response<Paging<Notice>> findNoticeShop(Long shopId, Integer type, Integer pageNo, Integer pageSize, String search, String startAt, String endAt);

    /**
     * 平台管理: 查询平台新闻/公告分页
     *
     * @param type 1:新闻 2:公告
     * @param status 状态，1->已发布2->停止
     * @param pageNo 页码
     * @param pageSize 大小
     * @param startAt 更新时间: 左区间
     * @param endAt 更新时间: 右区间
     * @return 新闻/公告分页
     */
    @Export(paramNames = {"type","status","pageNo","pageSize","startAt","endAt"})
    Response<Paging<Notice>> findNoticePlatformForManage(Integer type, Integer status, Integer pageNo, Integer pageSize, String startAt, String endAt);

    /**
     * 店铺首页展示: 查询店铺新闻/公告分页
     * @param shopId 店铺ID
     * @param type 1:新闻 2:公告
     * @param status 状态，1->已发布2->停止
     * @param pageNo 页码
     * @param pageSize 大小
     * @param startAt 更新时间: 左区间
     * @param endAt 更新时间: 右区间
     * @return 新闻/公告分页
     */
    @Export(paramNames = {"shopId","type","status","pageNo","pageSize","startAt","endAt"})
    Response<Paging<Notice>> findNoticeShopForManage(Long shopId, Integer type, Integer status, Integer pageNo, Integer pageSize, String startAt, String endAt);

    /**
     * 通过 ID 查询新闻/公告
     *
     * @param noticeId 新闻/公告 ID
     * @return 新闻/公告
     */
    @Export(paramNames = {"noticeId"})
    public Response<Notice> findById(Long noticeId);

}
