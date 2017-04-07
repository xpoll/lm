package cn.blmdz.rabbit.web.core.notice;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.rabbit.user.model.Notice;
import cn.blmdz.rabbit.user.service.NoticeReadService;
import cn.blmdz.rabbit.user.service.NoticeWriteService;
import cn.blmdz.wolf.common.model.ParanaUser;
import lombok.extern.slf4j.Slf4j;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
@RestController
@Slf4j
@RequestMapping("/api/notice")
public class NoticeController {
    private final NoticeWriteService noticeWriteService;
    private final NoticeReadService noticeReadService;

    @Autowired
    public NoticeController(NoticeWriteService noticeWriteService, NoticeReadService noticeReadService) {
        this.noticeWriteService = noticeWriteService;
        this.noticeReadService = noticeReadService;
    }

    @RequestMapping("/findById")
    public Notice findById(@RequestParam(value = "noticeId")Long noticeId){
        return noticeReadService.findById(noticeId).getResult();
    }

    @RequestMapping("/createNotice")
    public boolean createNewNotice(@RequestBody Notice notice){
        ParanaUser user= UserUtil.getCurrentUser();
        if(Arguments.isNull(user.getShopId())){
            notice.setLevel(2);
        }else{
            notice.setLevel(1);
            notice.setShopId(user.getShopId());
        }
        notice.setStatus(0);
        notice.setCreatorName(user.getName());
        notice.setCreatorId(user.getId());
        return noticeWriteService.createNotice(notice).getResult();
    }
    @RequestMapping("/deleteNotice")
    public boolean delete(@RequestParam("noticeId")Long noticeId){
        return noticeWriteService.deleteNotice(noticeId).getResult();
    }

    @RequestMapping("/updateStatus")
    public boolean updateStatus(@RequestParam("noticeId")Long noticeId,
                                @RequestParam("status")Integer status){
        return noticeWriteService.updateStatus(noticeId,status).getResult();
    }

    @RequestMapping("/updateNotice")
    public boolean updateNotice(@RequestBody Notice notice){
        ParanaUser user= UserUtil.getCurrentUser();
        if(Arguments.isNull(user.getShopId())){
            notice.setLevel(2);
        }else{
            notice.setLevel(1);
            notice.setShopId(user.getShopId());
        }
        notice.setCreatorName(user.getName());
        notice.setCreatorId(user.getId());
        return noticeWriteService.updateNoticeById(notice).getResult();
    }

}
