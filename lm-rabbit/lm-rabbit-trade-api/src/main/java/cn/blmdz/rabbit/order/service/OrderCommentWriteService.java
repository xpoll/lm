package cn.blmdz.rabbit.order.service;


import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.rabbit.order.model.OrderComment;

/**
 * Author:cp
 * Created on 4/22/16.
 */
public interface OrderCommentWriteService {

    /**
     * 添加订单评论
     *
     * @param orderComment 评论
     * @return 禁用词
     */
    Response<Map<Long, List<String>>> createOrderComment(OrderComment orderComment);

    /**
     * 物理删除评价
     *
     * @param orderCommentId 评价id
     * @return 是否操作成功
     */
    Response<Boolean> deleteOrderComment(Long orderCommentId);

    /**
     * 添加评论回复
     *
     * @param orderCommentId 评论id
     * @param reply          回复内容
     * @param loginerId      登录用户id
     * @return 是否添加成功
     */
    Response<List<String>> addCommentReply(Long orderCommentId, String reply, Long loginerId);

    /**
     * 批量修改订单评价状态
     *
     * @param orderCommentIds 订单评价id列表
     * @param status          评价状态
     * @return 是否修改成功
     */
    Response<Boolean> batchUpdateOrderComment(List<Long> orderCommentIds, Integer status);
}
