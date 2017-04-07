package cn.blmdz.rabbit.web.front.seller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Optional;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.rabbit.user.model.Seller;
import cn.blmdz.rabbit.user.service.SellerReadService;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Effet
 */
@Slf4j
@Service
public class SellerService {

    private final SellerReadService sellerReadService;

    @Autowired
    public SellerService(SellerReadService sellerReadService) {
        this.sellerReadService = sellerReadService;
    }

    @Export(paramNames = {"user"})
    public Response<Seller> findSellerByLoginUser(BaseUser user) {
        return orEmpty(sellerReadService.findSellerByUserId(user.getId()));
    }

    private Response<Seller> orEmpty(Response<Optional<Seller>> resp) {
        if (resp.isSuccess()) {
            return Response.ok(resp.getResult().or(new Seller()));
        }
        return Response.fail(resp.getError());
    }
}
