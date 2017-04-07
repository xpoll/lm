package cn.blmdz.rabbit.user.impl.service;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.home.common.util.DayRange;
import cn.blmdz.home.common.util.Params;
import cn.blmdz.rabbit.user.impl.dao.NoticeDao;
import cn.blmdz.rabbit.user.model.Notice;
import cn.blmdz.rabbit.user.service.NoticeReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
@Slf4j
@Service
public class NoticeReadServiceImpl implements NoticeReadService {

    private final NoticeDao noticeDao;
    @Autowired
    public NoticeReadServiceImpl(NoticeDao noticeDao) {
        this.noticeDao = noticeDao;
    }

    @Override
    public Response<Paging<Notice>> findNoticePlatform(Integer type, Integer pageNo, Integer pageSize, String search, String startAt, String endAt) {
        try {
            DayRange range = DayRange.from(startAt, endAt);
            PageInfo page = new PageInfo(pageNo, pageSize);

            Map<String, Object> params = new HashMap<>();
            params.put("type", type);
            params.put("level", 2);
            params.put("search", Params.trimToNull(search));
            params.put("status", 1);
            params.putAll(range.toMap("startAt", "endAt"));
            params.putAll(page.toMap());
            return Response.ok(noticeDao.paging(params));
        } catch (Exception e) {
            log.error("find plant notice failed, error = {}", Throwables.getStackTraceAsString(e));
            return Response.fail("plant.page.find.failed");
        }
    }

    @Override
    public Response<Paging<Notice>> findNoticeShop(Long shopId, Integer type, Integer pageNo, Integer pageSize, String search, String startAt, String endAt) {
        try {
            if(Arguments.isNull(shopId)){
               return Response.fail("shopId.is.null");
            }
            DayRange range = DayRange.from(startAt, endAt);
            PageInfo page = new PageInfo(pageNo, pageSize);

            Map<String, Object> params = new HashMap<>();
            params.put("type", type);
            params.put("level", 1);
            params.put("shopId", shopId);
            params.put("search", Params.trimToNull(search));
            params.put("status", 1);
            params.putAll(range.toMap("startAt", "endAt"));
            params.putAll(page.toMap());
            return Response.ok(noticeDao.paging(params));
        } catch (Exception e) {
            log.error("find plant notice failed, error = {}", Throwables.getStackTraceAsString(e));
            return Response.fail("plant.page.find.failed");
        }
    }

    @Override
    public Response<Paging<Notice>> findNoticePlatformForManage(Integer type, Integer status, Integer pageNo, Integer pageSize, String startAt, String endAt) {
        try {
            DayRange range = DayRange.from(startAt, endAt);
            PageInfo page = new PageInfo(pageNo, pageSize);

            Map<String, Object> params = new HashMap<>();
            params.put("type", type);
            params.put("level", 2);
            params.put("status", status);
            params.putAll(range.toMap("startAt", "endAt"));
            params.putAll(page.toMap());
            return Response.ok(noticeDao.paging(params));
        } catch (Exception e) {
            log.error("find plant notice failed, error = {}", Throwables.getStackTraceAsString(e));
            return Response.fail("plant.page.find.failed");
        }
    }

    @Override
    public Response<Paging<Notice>> findNoticeShopForManage(Long shopId, Integer type, Integer status, Integer pageNo, Integer pageSize, String startAt, String endAt) {
        try {
            if(Arguments.isNull(shopId)){
                return Response.fail("shopId.is.null");
            }
            DayRange range = DayRange.from(startAt, endAt);
            PageInfo page = new PageInfo(pageNo, pageSize);

            Map<String, Object> params = new HashMap<>();
            params.put("type", type);
            params.put("level", 1);
            params.put("shopId", shopId);
            params.put("status", status);
            params.putAll(range.toMap("startAt", "endAt"));
            params.putAll(page.toMap());
            return Response.ok(noticeDao.paging(params));
        } catch (Exception e) {
            log.error("find plant notice failed, error = {}", Throwables.getStackTraceAsString(e));
            return Response.fail("plant.page.find.failed");
        }
    }

    @Override
    public Response<Notice> findById(Long noticeId) {
        try {
            return Response.ok(noticeDao.findById(noticeId));
        }catch(Exception e) {
            log.error("find all notice failed, cause:{}",Throwables.getStackTraceAsString(e));
            return Response.fail("notice.find.fail");
        }
    }

}
