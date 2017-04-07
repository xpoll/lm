package cn.blmdz.rabbit.user.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.user.model.Notice;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
public interface NoticeWriteService {

    /**
     * 创建公告信息
     * @param notice   公告定义
     * @return  Boolean
     * 返回创建结果
     */
    public Response<Boolean> createNotice(Notice notice);

    /**
     * 删除公告信息
     * @param noticeId     公告编号
     * @return  Boolean
     * 返回删除是否成功
     */
    public Response<Boolean> deleteNotice(Long noticeId);
    /**
     * 更新公告状态信息
     * @param noticeId   公告编号
     * @param status    优惠码更改信息
     * @return  Boolean
     * 返回更改是否成功
     */
    public Response<Boolean> updateStatus(Long noticeId, Integer status);

    /**
     * 更新公告内容
     * @param notice
     * @return
     */
    public Response<Boolean> updateNoticeById(Notice notice);

    public Response<Boolean> disable(Long noticeId);
}
